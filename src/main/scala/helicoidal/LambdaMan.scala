package helicoidal

enum Dir {
  case U, D, L, R

  def opposite: Dir =
    this match {
      case U => D
      case D => U
      case L => R
      case R => L
    }
}

final case class Point(x: Int, y: Int) {
  def west: Point = copy(x - 1, y)
  def east: Point = copy(x + 1, y)
  def north: Point = copy(x, y - 1)
  def south: Point = copy(x, y + 1)

  def within(point: Point): Boolean =
    (x >= 0 && x < point.x) && (y >= 0 && y < point.y)

  override def toString: String = s"($x,$y)"
}

final case class Edge(src: Point, dst: Point, dir: Dir) {
  override def toString: String = s"$src → $dst [$dir]"

}

final case class Graph private (adj: Map[Point, List[Edge]]) {
  def add(edge: Edge): Graph =
    Graph {
      adj.updatedWith(edge.src) { edges =>
        Some(edge :: edges.getOrElse(List.empty))
      }
    }

  def dfs(start: Point): String = {
    var marked = Set.empty[Point]
    var path = Vector.empty[Dir]

    def recur(src: Point): Unit = {
      marked += src

      adj(src).foreach { case edge @ Edge(src, dst, dir) =>
        println(edge)

        if marked.contains(dst)
        then ()
        else {
          path :+= dir
          recur(dst)
          path :+= dir.opposite
        }
      }
    }

    recur(start)

    path.mkString
  }

  override def toString: String = {
    val str = new StringBuilder

    adj.foreach { (src, edges) =>
      str ++= s"$src:\n"

      edges.foreach { case Edge(src, dst, dir) =>
        str ++= s"   $dst [$dir]\n"
      }
    }

    str.result()
  }
}

object Graph {
  val empty: Graph = Graph(Map.empty)
}

object LambdaMan {
  def main(args: Array[String]): Unit = {
    // val input = """###.#...
    //   |...L..##
    //   |.#######""".stripMargin

    val input =
      """L..........#..........#..........#..........#.....
        |.....#..........#..........#..........#..........#
        |..........#..........#..........#..........#......
        |....#..........#..........#..........#..........#.
        |.........#..........#..........#..........#.......
        |...#..........#..........#..........#..........#..
        |........#..........#..........#..........#........
        |..#..........#..........#..........#..........#...
        |.......#..........#..........#..........#.........
        |.#..........#..........#..........#..........#....
        |......#..........#..........#..........#..........
        |#..........#..........#..........#..........#.....
        |.....#..........#..........#..........#..........#
        |..........#..........#..........#..........#......
        |....#..........#..........#..........#..........#.
        |.........#..........#..........#..........#.......
        |...#..........#..........#..........#..........#..
        |........#..........#..........#..........#........
        |..#..........#..........#..........#..........#...
        |.......#..........#..........#..........#.........
        |.#..........#..........#..........#..........#....
        |......#..........#..........#..........#..........
        |#..........#..........#..........#..........#.....
        |.....#..........#..........#..........#..........#
        |..........#..........#..........#..........#......
        |....#..........#..........#..........#..........#.
        |.........#..........#..........#..........#.......
        |...#..........#..........#..........#..........#..
        |........#..........#..........#..........#........
        |..#..........#..........#..........#..........#...
        |.......#..........#..........#..........#.........
        |.#..........#..........#..........#..........#....
        |......#..........#..........#..........#..........
        |#..........#..........#..........#..........#.....
        |.....#..........#..........#..........#..........#
        |..........#..........#..........#..........#......
        |....#..........#..........#..........#..........#.
        |.........#..........#..........#..........#.......
        |...#..........#..........#..........#..........#..
        |........#..........#..........#..........#........
        |..#..........#..........#..........#..........#...
        |.......#..........#..........#..........#.........
        |.#..........#..........#..........#..........#....
        |......#..........#..........#..........#..........
        |#..........#..........#..........#..........#.....
        |.....#..........#..........#..........#..........#
        |..........#..........#..........#..........#......
        |....#..........#..........#..........#..........#.
        |.........#..........#..........#..........#.......
        |...#..........#..........#..........#..........#..""".stripMargin

    val grid = input.split('\n').map(_.toArray)

    val dirs = Map[Dir, Point => Point](
      Dir.U -> (_.north),
      Dir.D -> (_.south),
      Dir.R -> (_.east),
      Dir.L -> (_.west),
    )

    val (graph, start) =
      grid.zipWithIndex.foldLeft[(Graph, Point)]((Graph.empty, null)) {
        case ((graph, start), (line, y)) =>
          val bounds = Point(line.length, grid.length)

          line.zipWithIndex.foldLeft((graph, start)) { case ((graph, start), (char, x)) =>
            val point = Point(x, y)

            char match {
              case '#' => (graph, start)

              case '.' | 'L' =>
                val newGraph =
                  dirs.foldLeft(graph) { case (graph, (dir, neighbor)) =>
                    val dst = neighbor(point)
                    if dst.within(bounds) && grid(dst.y)(dst.x) != '#'
                    then graph.add(Edge(point, dst, dir))
                    else graph
                  }

                if char == 'L'
                then (newGraph, point)
                else (newGraph, start)

              case chr => sys.error(s"unexpected char: $chr")
            }
          }
      }

    println(input)
    // println(start)
    // println(graph)
    println(graph.dfs(start))
  }
}
