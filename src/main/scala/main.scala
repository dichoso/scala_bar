import akka.actor.{ ActorRef, ActorSystem, Props, Actor, Inbox }
import scala.concurrent.duration._
import akka.event.Logging


case class ActorFound(actor: ActorRef)
case class Conso(conso: String)
case class Order(order: String)
case class Conso2(conso: String, client: ActorRef)
case class Order2(order: String, client: ActorRef)
case object FindABarman
case object FindAServer
case object TheEnd

class Bar extends Actor {
  // le journal du bar
  val log = Logging(context.system, this)

  // Ce bar a besoin d'un barman
   val barman1 = context.actorOf(Props[Barman])

  // Ce bar a aussi besoin d'un serveur
  /* création d'un acteur avec paramètre */
  val monpremierserveur = context.actorOf(Props(new Server(barman1, "toto")))

  def receive = {
    case FindABarman => {
      log.info("on demande un barman")
      sender ! ActorFound(barman1)
    }
    case FindAServer => {
      log.info("on demande un serveur")
      sender ! ActorFound(monpremierserveur)
    }
    case TheEnd => context stop self
  }
}

class Barman() extends Actor {
  val log = Logging(context.system, this)
  def receive = {
    case Order2(conso, client) => {
      log.info(s"conso: $conso")
      sender ! Conso2(conso, client)
    }
  }
}

class Server(barman: ActorRef, name: String) extends Actor {
  def receive = {
    case Order(conso) => {
      barman ! Order2(conso, sender)
    }
    case Conso2(conso, client) => client ! Conso(conso)
  }
}

class Customer(bar: ActorRef, conso: String) extends Actor {
   val log = Logging(context.system, this)
  override def preStart = {
    bar ! FindAServer
  }
  def receive = {
    case ActorFound(a) => a ! Order(conso)
    case Conso(c) => log.info(s"merci pour le $c")
  }
}

object BarAvenir extends App {
  val system = ActorSystem("bar-des-acteurs")
  val avenir = system.actorOf(Props[Bar], "Avenir")

  val commands = Vector("tisane", "chocolat", "thé", "café")
  (1 to 1000).foreach { (i) => {
      system.actorOf(Props( new Customer(avenir, commands(i % 4)) ), s"client-$i")
    }
  }
}
