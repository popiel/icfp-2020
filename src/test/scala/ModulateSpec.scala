import foo._

import org.scalatest._
import org.scalatest.funspec._
import org.scalatest.matchers._

import Modulate._

class ModulateSpec extends AnyFunSpec with should.Matchers {
  describe("modulate") {
    describe("numbers") {
      it("should properly encode 0") { modulate(0) shouldBe "010" }
      it("should properly encode 1") { modulate(1) shouldBe "01100001" }
      it("should properly encode 2") { modulate(2) shouldBe "01100010" }
      it("should properly encode 17") { modulate(17) shouldBe "0111000010001" }
      it("should properly encode -1") { modulate(-1) shouldBe "10100001" }
    }
    describe("lists") {
      it("should properly encode nil") { modulate(Nil) shouldBe "00" }
      it("should properly encode cons") {
        modulate((0, 1)) shouldBe "1101001100001"
      }
      it("should properly encode a nested cons") {
        modulate((1, (2, Nil))) shouldBe "1101100001110110001000"
      }
      it("should properly encode a short list") {
        modulate(List(1, 2)) shouldBe "1101100001110110001000"
      }
    }
  }
  describe("demodulate") {
    describe("numbers") {
      it("should properly decode 0") { demodulate("010") shouldBe (0, "") }
      it("should properly decode 1") { demodulate("01100001") shouldBe (1, "") }
      it("should properly decode 2") { demodulate("01100010") shouldBe (2, "") }
      it("should properly decode 17") { demodulate("0111000010001") shouldBe (17, "") }
      it("should properly decode -1") { demodulate("10100001") shouldBe (-1, "") }
    }
    describe("lists") {
      it("should properly decode nil") { demodulate("00") shouldBe (Nil, "") }
      it("should properly decode cons") {
        demodulate("1101001100001") shouldBe ((0, 1), "")
      }
      it("should properly decode a short list") {
        demodulate("1101100001110110001000") shouldBe (List(1, 2), "")
      }
    }
  }

  describe("interpreter") {
    import AST.extract
    it("cons should properly build a list") {
      val interp = new Interpreter()
      interp.run(":1029 = ap ap cons 7 ap ap cons 8 nil")
      extract[Any](interp.symbols(":1029")).toString() shouldBe "List(7, 8)"
    }

    it("recursion should not explode") {
      val interp = new Interpreter()
      interp.run(":2048 = ap f :2048")
      interp.run(":test = ap :2048 42")
      extract[Any](interp.symbols(":test")).toString() shouldBe "42"
    }

    it("s should work") {
      val interp = new Interpreter()
      interp.run(":test = ap ap ap s add inc 1")
      extract[Any](interp.symbols(":test")).toString() shouldBe "3"
      interp.run(":test2 = ap ap ap s mul ap add 1 6")
      extract[Any](interp.symbols(":test2")).toString() shouldBe "42"
    }

    it("c should work") {
      val interp = new Interpreter()
      interp.run(":test = ap ap ap c add 1 2")
      extract[Any](interp.symbols(":test")).toString() shouldBe "3"
      interp.run(":test2 = ap ap ap c div 2 6")
      extract[Any](interp.symbols(":test2")).toString() shouldBe "3"
    }

    it("b should work") {
      val interp = new Interpreter()
      interp.run(":test = ap ap ap b inc dec 2")
      extract[Any](interp.symbols(":test")).toString() shouldBe "2"
    }

    it("t should work") {
      val interp = new Interpreter()
      interp.run(":test = ap ap t 1 2")
      extract[Any](interp.symbols(":test")).toString() shouldBe "1"
    }

    it("f should work") {
      val interp = new Interpreter()
      interp.run(":test = ap ap f 1 2")
      extract[Any](interp.symbols(":test")).toString() shouldBe "2"
    }

    it("pow2") {
      val interp = new Interpreter()
      interp.run(":test = ap pwr2 2")
      extract[Any](interp.symbols(":test")).toString() shouldBe "4"
    }

    it("i") {
      val interp = new Interpreter()
      interp.run(":test = ap i 2")
      extract[Any](interp.symbols(":test")).toString() shouldBe "2"
    }
  }

  describe("interacting galaxy") {
    ignore("should initialize properly") {
      Interact.interact(BigInt(0), Nil) shouldBe ""
    }
  }
}
