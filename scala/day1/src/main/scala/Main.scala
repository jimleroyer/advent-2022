import scala.io.Source
import scala.util.Using

val PATH_INPUT = "data/input.txt"

type Calorie = Int
type Calories = List[Calorie]

def readInput(filename: String): Calories = {
  val meals    = Using(Source.fromFile(filename)) { s => s.mkString.split("\n\n") }.get
  val calories = meals.map(parseCalorie).toList
  calories
}

def parseCalorie(meals: String): Calorie = {
  val strMeals = meals.split("\n")
  val intMeals = strMeals.map(_.toInt)
  val calorie  = intMeals.sum
  calorie
}

@main def day1: Unit =
  val calories: Calories = readInput(PATH_INPUT)

  // Part 1 - top 1 hoarding elf with most calories
  val topCalories = calories.sorted(Ordering[Int].reverse)
  val topOneC     = topCalories.head
  println(s"Total calories for the #1 hoarding elf is $topOneC")

  // Part 2 - top 3 hoarding elves and their calories
  val topThreeC = topCalories
    .take(3)
    .sum
  println(s"Total calories for the top 3 hoarding elves is $topThreeC")
