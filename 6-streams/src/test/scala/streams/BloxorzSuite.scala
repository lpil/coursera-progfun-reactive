package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left  => block.left
        case Right => block.right
        case Up    => block.up
        case Down  => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  //
  // Terrain function
  //
  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0 o")
      assert(terrain(Pos(0,2)), "0,2 o")
      assert(terrain(Pos(1,1)), "1,1 S")
      assert(terrain(Pos(4,7)), "4,7 T")
      assert(!terrain(Pos(0,3)), "0,3 -")
      assert(!terrain(Pos(4,4)), "4,4 -")
      assert(!terrain(Pos(4,11)), "4,11 out of bounds")
      assert(!terrain(Pos(10,1)), "10,1 out of bounds")
    }
  }

  //
  // findChar
  //
  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      assert(goal == Pos(4,7))
    }
  }

  //
  // Block isStanding
  //
  test("block isStanding") {
    new Level1 {
      assert(Block(Pos(0,0),Pos(0,0)).isStanding)
      assert(Block(Pos(10,10),Pos(10,10)).isStanding)
      assert(!Block(Pos(20,11),Pos(30,11)).isStanding)
      assert(!Block(Pos(0,0),Pos(0,1)).isStanding)
    }
  }

  //
  // Block isLegal
  //
  test("block isLegal") {
    new Level1 {
      assert(Block(Pos(0,0),Pos(0,0)).isLegal, "0,0 0,0")
      assert(Block(Pos(0,0),Pos(1,0)).isLegal, "0,0 1,0")
      assert(Block(Pos(3,1),Pos(3,2)).isLegal, "3,1 3,2")
      assert(!Block(Pos(3,0),Pos(3,1)).isLegal, "3,0 3,1")
      assert(!Block(Pos(5,0),Pos(5,0)).isLegal, "5,0 5,0")
    }
  }

  //
  // Block startBlock
  //
  test("block startBlock") {
    new Level1 {
      assert(startBlock === Block(Pos(1,1), Pos(1,1)))
    }
  }

  ignore("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  ignore("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
