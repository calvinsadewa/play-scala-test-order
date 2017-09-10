package extensions

import scala.concurrent.{ExecutionContext, Future}

object NiceFutureEither {
  class EitherFuture[A,B] (problem: Either[A,Future[B]]) {
    def toFuture()(implicit ec:ExecutionContext) = problem match {
      case Left(x) => Future(Left(x))
      case Right(futB) => futB.map(b => Right(b))
    }
  }

  implicit def niceFuture[A,B] (problem: Either[A,Future[B]]) = new EitherFuture(problem)

}
