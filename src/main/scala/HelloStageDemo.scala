import scalafx.scene.shape.{Circle, Polygon}
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color._
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps


object Nadzorca {
  case class dodajPunkt(x: Double,y: Double)
  case object Zrobione
  case class Wyslij(listaOtoczki: List[(Double,Double,ActorRef)])
}

object Punkt {
  case class obliczKaty(a: Tuple3[Double,Double,ActorRef])
  case class podajNastepny(a: Double)
  case class jestemNowy(x: Double,y: Double,listaOtoczki: List[(Double,Double,ActorRef)])
  case class wysylamNowego(x: Double,y: Double,a: ActorRef)
  case object DoDziela
  case class Nastepny(kat: Double,listaPunktow: List[(Double,Double, ActorRef)],pierwszy: ActorRef)
}

class Nadzorca extends Actor {
  import Nadzorca._
  import Punkt._
  def receive: Receive = {
    case dodajPunkt(x,y) => // dodanie pierwszego punktu
      val punkt = context.actorOf(Props[Punkt]) // dodanie nowego aktora reprezentującego punkt
      context.become(wyslijOtoczke(sender)) // zmiana tożsamości aktora na funkcję wysyłająco listę punktów otoczki
      self ! Wyslij(List((x,y,punkt))) // wysłanie komendy każącą wysłać otoczkę
      punkt ! jestemNowy(x,y,List()) // wysłanie do aktora "punkt" współrzędnych
  }
  def wyslijOtoczke(rysunek: ActorRef): Receive = {
    case Wyslij(listaOtoczki) =>
      rysunek ! listaOtoczki // aktor wysyła do rysunku (aktora tymczasowego stworzonego na potrzeby zapyyania w interfejsie graficznym)
        .map {
          case (x,y,_) => (x,-y) // odwrócenie współrzędnych y i usunięcie aktora
        }
      println(listaOtoczki)
      context.become(wyznaczOtoczke(listaOtoczki)) // zmiana tożsamości na wyznaczanie otoczki
    case _ => println("blad")
  }
  def wyznaczOtoczke(listaOtoczki: List[(Double,Double,ActorRef)]): Receive = {
    case dodajPunkt(x,y) =>
      val punkt = context.actorOf(Props[Punkt]) // dodanie nowego punktu
      println(x,y)
      punkt ! jestemNowy(x,y,listaOtoczki) // wysłanie do punktu współrzędnych x i y
      def Helper(listaOtoczki: List[(Double,Double,ActorRef)]): Unit = { // wysłanie do reszty punktów z otoczki nowego punktu
        listaOtoczki match {
          case (_,_,a)::tail =>
            a ! wysylamNowego(x,y,punkt)
            Helper(tail)
          case List() => ()
        }
      }
      context.become(Wyznaczam(sender,listaOtoczki.length, ((x,y,punkt)::listaOtoczki).minBy(_._2)._3)) // przejście do stanu wyznaczania
      Helper(listaOtoczki)
    case _ => println("blad")
  }
  def Wyznaczam(rysunek: ActorRef, pozostalo: Int, pierwszy: ActorRef): Receive = {
    case Zrobione => // odbiór komunikatu Zrobione - pomniejsza liczbę pozostałych aktorów o 1
      pozostalo match {
        case a if a > 0 =>
          context.become(Wyznaczam(rysunek,pozostalo-1,pierwszy))
        case _ => pierwszy ! DoDziela
          context.become(wyslijOtoczke(rysunek))
      }

  }
}

class Punkt extends Actor {
  import Punkt._
  import Nadzorca._
  import HelloStageDemo.Angle
  def receive: Receive = {
    case jestemNowy(x,y,listaOtoczki) => // odbiór komunikatu jestemNowy
      val listaKatow = listaOtoczki
        .map {
          case (x1,y1,a1) => (math.atan2(y1 - y,x1 - x).value,a1) // oblicza kąt wektora pomiędzy punktami z otoczki
        }
  println(s"Nowy: $listaKatow")
      context.become(Praca(x,y,listaKatow, sender)) // zmiana tożsamości na pracę
      sender ! Zrobione // wysyłanie do nadzorcy informacji o wykonaniu zadania

  }
  def Praca(x: Double,y: Double, listaOtoczki: List[(Double, ActorRef)], rodzic: ActorRef): Receive = {
    case DoDziela => // komunikat DoDziela wykonywany jest dla punktu o najmniejszej współrzędnej y
      val najmniejszy = listaOtoczki // wyznaczenie punktu z najmniejszym kątem wektora tworzonego z pierwszym punktem
        .minBy(_._1)
      println(s"Zaczynam $self ${najmniejszy._1}")
      najmniejszy._2 ! Nastepny(najmniejszy._1, List((x,y,self)),self) // wysłanie do wyznaczonego punktu współrzędnych punktu
    case Nastepny(kat, listaPunktow, pierwszy) => // odbiór komunikatu następny wyznaczającego na podstawie ostatniego kąta nowy punkt
      pierwszy match {
        case `self` => rodzic ! Wyslij(listaPunktow) // jeśli aktor jest tożsamy z pierwszym punktem aktor wysyła do nadzorcy polecenie wysłania otoczki
        case _ =>
          val najmniejszy = listaOtoczki // wyznaczenie punktu z najmniejszą różnicą kątów (kąt zawsze ma wartość dodatnią)
            .map {
              case (ang, a) => ((ang-kat).value,a,ang)
            }
            .minBy(_._1)
          println(s"kontynuuje $self ${najmniejszy._1}")
          najmniejszy._2 ! Nastepny(najmniejszy._3, (x,y,self)::listaPunktow,pierwszy) // wysłanie do następnego punktu listy otoczki oraz kątu
      }
    case wysylamNowego(x1, y1, a1) => // komunikat wprowadzający nowy punkt do listy otoczki
      context.become(Praca(x,y,(math.atan2(y1 - y,x1 - x).value,a1)::listaOtoczki,rodzic))
      println(s"Stary ${(math.atan2(y1 - y,x1 - x).value,a1)::listaOtoczki}")
      sender ! Zrobione // wysyłanie do nadzorcy informacji o wykonaniu zadania
  }

}

object Circle {
  def apply(x: Double, y: Double) = new Circle() {
    centerX = x
    centerY = y
    radius = 5
    fill <== when(hover) choose Green otherwise Red
  }
}


object HelloStageDemo extends JFXApp {

  import Nadzorca._
  stage = new JFXApp.PrimaryStage { // tworzenie grafiki 1000 na 1000 pikseli
    title.value = "Hello Stage"
    width = 1000
    height = 1000
    scene = new Scene {
      fill = LightGreen
      content = List(Polygon())
      onMouseClicked = (me: MouseEvent) => { // reakcja na kliknięcie kursorem w określonym punkcie
        content += Circle(me.x,me.y)
        implicit val timeout = Timeout(5 seconds)
        val future = szef ? dodajPunkt(me.x, -me.y) // wysłanie zapytania do nadzorcy
        val result = Await.result(future, timeout.duration) // odebranie otoczki wypukłej
        content.remove(0) // usunięcie starej otoczki
        result match {
          case a: List[(Double,Double)] => a.toPolygon +=: content // doodanie nowej otoczki (przetworzenie listy na wielokąt)
        }
      }
    }
  }


 implicit class Angle(kat: Double) { // wyzaczanie kąta tak aby miał dodatnią wartość
   def value: Double = {
     kat match {
       case a if a >=0 && a < math.Pi*2 => a
       case a if a < 0 => a % (- math.Pi * 2) match {
         case 0 => 0
         case a => a + math.Pi * 2
       }
       case _ => kat % (math.Pi * 2)
     }
   }
 }
  implicit class Poli(punkty: List[(Double,Double)]) { // konwersja listy Tuple na wielokąt
    def toPolygon: Polygon = {
      def Helper(punkty: List[(Double, Double)], poli: Polygon): Polygon = {
        punkty match {
          case (x,y)::tail =>
            poli.getPoints.addAll(x,y)
            Helper(tail,poli)
          case List() =>
            poli
        }
      }
      val res = Helper(punkty, Polygon())
      res.stroke = Black
      res.fill = Transparent
      res
    }
  }
  val system = ActorSystem("Punkty")
  val szef = system.actorOf(Props[Nadzorca], "szef")
}