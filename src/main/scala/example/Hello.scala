package example

import scala.concurrent.ExecutionContext
import org.http4s.server.blaze.BlazeServerBuilder
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.data.Kleisli
import cats.data.EitherT
import sttp.tapir.Endpoint

import io.circe.generic.auto._
import sttp.client.SttpBackend
import sttp.model.Uri
import cats.data.NonEmptyList
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.http4s.Http4sServerOptions
import cats.effect.Sync
import cats.effect.ContextShift
import org.http4s.client.blaze.BlazeClientBuilder
import sttp.client.http4s.Http4sBackend
import cats.effect.Blocker
import org.http4s.server.Server

//
//
//
//
//
//generic stuff
//
//
//
//
//
//
trait FunK3[F[_, _, _], G[_, _, _]] {
  def apply[A, B, C](f: F[A, B, C]): G[A, B, C]
}

final case class TupleK3[F[_, _, _], G[_, _, _], A, B, C](
    left: F[A, B, C],
    right: G[A, B, C]
)

trait ApplyK3[Alg[_[_, _, _]]] {
  def mapK3[F[_, _, _], G[_, _, _]](f1: Alg[F])(fk: FunK3[F, G]): Alg[G]
  def applyK3[F[_, _, _], G[_, _, _], H[_, _, _]](f1: Alg[F], f2: Alg[G])(
      fk: FunK3[TupleK3[F, G, *, *, *], H]
  ): Alg[H]
}

trait ListAll[Alg[_[_, _, _]]] {
  def listAll[F[_, _, _]](alg: Alg[F]): NonEmptyList[F[_, _, _]]
}

//
//
//
//
//
//
//common library code
//
//
//
//
//
//

object commons {
  type Impl[F[_]] = { type L[I, E, O] = Kleisli[EitherT[F, E, *], I, O] }

  def zipWithLogic[Router[_[_, _, _]], F[_]](
      endpoints: Router[Endpoint[*, *, *, Nothing]],
      implementations: Router[Impl[F]#L]
  )(implicit apply: ApplyK3[Router]) = {

    apply.applyK3(endpoints, implementations)(
      new FunK3[
        TupleK3[Endpoint[*, *, *, Nothing], Impl[F]#L, *, *, *],
        ServerEndpoint[*, *, *, Nothing, F]
      ] {
        def apply[A, B, C](
            f: TupleK3[Endpoint[*, *, *, Nothing], Impl[F]#L, A, B, C]
        ): ServerEndpoint[A, B, C, Nothing, F] =
          f.left.serverLogic(f.right.run.andThen(_.value))
      }
    )
  }

  def routes[Router[_[_, _, _]], F[_]: Sync: ContextShift](
      router: Router[ServerEndpoint[*, *, *, Nothing, F]]
  )(
      implicit listAll: ListAll[Router],
      serverOptions: Http4sServerOptions[F]
  ) = {
    import sttp.tapir.server.http4s._

    listAll.listAll(router).toList.toRoutes
  }

  import sttp.client.NothingT

  def sttpClient[Router[_[_, _, _]], F[_]](
      uri: Uri,
      endpoints: Router[Endpoint[*, *, *, Nothing]],
      backend: SttpBackend[F, Nothing, NothingT]
  )(implicit apply: ApplyK3[Router]): Router[Impl[F]#L] =
    apply.mapK3(endpoints)(new FunK3[Endpoint[*, *, *, Nothing], Impl[F]#L] {

      def apply[A, B, C](f: Endpoint[A, B, C, Nothing]): Impl[F]#L[A, B, C] =
        runEndpoint(uri, backend, f)
    })

  private def runEndpoint[F[_], I, E, O](
      uri: Uri,
      backend: SttpBackend[F, Nothing, NothingT],
      endpoint: Endpoint[I, E, O, Nothing]
  ): Impl[F]#L[I, E, O] = Kleisli { input =>
    import sttp.tapir.client.sttp._

    EitherT(
      backend.responseMonad.map(
        backend.send(endpoint.toSttpRequestUnsafe(uri).apply(input))
      )(_.body)
    )
  }

}

trait Boilerplate {

  //
  //
  //
  //
  //
  //
  //boilerplate - should be generated automatically
  //
  //
  //
  //
  //
  //
  implicit val applyK3: ApplyK3[InventoryRoutes] =
    new ApplyK3[InventoryRoutes] {
      def mapK3[F[_, _, _], G[_, _, _]](
          f1: InventoryRoutes[F]
      )(fk: FunK3[F, G]): InventoryRoutes[G] = new InventoryRoutes[G] {
        val createInventory: G[
          CreateInventoryCommand,
          CreateInventoryError,
          CreateInventoryResult
        ] = fk(f1.createInventory)
      }

      def applyK3[F[_, _, _], G[_, _, _], H[_, _, _]](
          f1: InventoryRoutes[F],
          f2: InventoryRoutes[G]
      )(fk: FunK3[TupleK3[F, G, *, *, *], H]): InventoryRoutes[H] =
        new InventoryRoutes[H] {
          val createInventory: H[
            CreateInventoryCommand,
            CreateInventoryError,
            CreateInventoryResult
          ] = fk.apply(TupleK3(f1.createInventory, f2.createInventory))
        }
    }

  implicit val listAllRoutes: ListAll[InventoryRoutes] =
    new ListAll[InventoryRoutes] {
      def listAll[F[_, _, _]](
          alg: InventoryRoutes[F]
      ): NonEmptyList[F[_, _, _]] = NonEmptyList.of(alg.createInventory)
    }
}

//
//
//
//
//
//
//user code
//
//
//
//
//
//

final case class CreateInventoryCommand(skuId: Long, amount: Int)
final case class CreateInventoryResult(inventoryId: Long)
final case class CreateInventoryError(errorCode: Int)

import commons.Impl

trait InventoryRoutes[E[_, _, _]] {
  def createInventory
      : E[CreateInventoryCommand, CreateInventoryError, CreateInventoryResult]
}

trait InventoryService[F[_]] {
  def createInventory(
      cmd: CreateInventoryCommand
  ): F[Either[CreateInventoryError, CreateInventoryResult]]
}

object InventoryRoutes extends Boilerplate {
  //business logic - implementation of server
  def serverImpl[F[_]](
      service: InventoryService[F]
  ): InventoryRoutes[Impl[F]#L] = new InventoryRoutes[Impl[F]#L] {

    val createInventory: Impl[F]#L[
      CreateInventoryCommand,
      CreateInventoryError,
      CreateInventoryResult
    ] = Kleisli { input => EitherT(service.createInventory(input)) }
  }

  //description of endpoint
  val endpoints: InventoryRoutes[Endpoint[*, *, *, Nothing]] =
    new InventoryRoutes[Endpoint[*, *, *, Nothing]] {
      val createInventory: Endpoint[
        CreateInventoryCommand,
        CreateInventoryError,
        CreateInventoryResult,
        Nothing
      ] = {
        import sttp.tapir._
        import sttp.tapir.json.circe._

        infallibleEndpoint
          .in("inventory")
          .post
          .in(jsonBody[CreateInventoryCommand])
          .out(jsonBody[CreateInventoryResult])
          .errorOut(jsonBody[CreateInventoryError])
      }
    }
}

object Hello extends IOApp {
  import sttp.tapir.docs.openapi._
  import sttp.tapir.openapi.circe.yaml._

  import cats.implicits._

  import org.http4s.implicits._

  val impl: InventoryService[IO] = i =>
    IO(println("demo")).as(Right(CreateInventoryResult(42L)))

  val routez = {
    import commons._

    routes(
      zipWithLogic(InventoryRoutes.endpoints, InventoryRoutes.serverImpl(impl))
    )
  }

  println(
    InventoryRoutes.listAllRoutes
      .listAll(InventoryRoutes.endpoints)
      .toList
      .toOpenAPI("a", "version")
      .toYaml
  )

  def runClient(server: Server[IO]) = {
    val serverUri = Uri
      .parse(server.baseUri.renderString)
      .leftMap(s => throw new Throwable(s))
      .merge

    Blocker[IO]
      .flatMap(Http4sBackend.usingDefaultClientBuilder[IO](_))
      .map { backend =>
        commons.sttpClient(
          serverUri,
          InventoryRoutes.endpoints,
          backend
        )
      }
  }

  val runServer = BlazeServerBuilder[IO](ExecutionContext.global)
    .bindHttp(8080, "0.0.0.0")
    .withHttpApp(routez.orNotFound)
    .resource

  def run(args: List[String]): IO[ExitCode] =
    runServer
      .flatMap(runClient)
      .use { client =>
        client.createInventory
          .run(CreateInventoryCommand(100L, 10))
          .value
      }
      .flatMap { result => IO(println(result)) }
      .as(ExitCode.Success)
}
