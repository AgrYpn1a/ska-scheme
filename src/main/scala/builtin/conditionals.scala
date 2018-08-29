package skascheme {
  object Conditionals {

    // (if (c) e e1)
    Interpreter.default.env += (SSymbol("if") -> ((args:Seq[SExpr]) => {
      if(args.size > 3)
        err("if: bad syntax.")
      else {
        val cond = args(0) match {
          case l: SList => l
          case _ => err("if: Invalid arguments.")
        }

        val ifTrue = args(1)
        val ifFalse = args(2)

        // Check for invalid args error
        if(isErr(cond))
          cond
        else {
          // cond is now condition to check, t/f
          Interpreter.default.reduce(cond) match {
            case c: SLogical => c match {
              case STrue => Interpreter.default.reduce(ifTrue)
              case SFalse => Interpreter.default.reduce(ifFalse)
              case _ => throw new Exception("Should never happen!")
            }

              case _ => err("if: Invalid condition, expected boolean.")
          }
        }

      }
    }))

    Interpreter.default.env += (SSymbol("cond") -> ((args: Seq[SExpr]) => ???))

    /** Bool checks */
    Interpreter.default.env += (SSymbol("=") -> ((args: Seq[SExpr]) => {
      var last = args(0)
      args.foldLeft(true)((acc, next) => {
          last == next
        }) match {
        case true => STrue
        case false => SFalse
      }
    }))

  }
}
