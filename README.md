# Ska Scheme
Scheme interpreter written in Scala, based on PC Scheme 2.0.

### Development progress

#### Core
- [x] Core interpreter

#### Core builtin functions
- [x] Function definitions, `define`
- [x] Quoting with `quote`
- [x] `let` environment expander
- [x] `let*` incremental environment expander
- [ ] `letrec` recursive environment expander

#### Conditional expressions
- [x] `if` expression
- [ ] `cond` expression

#### Lists
- [ ] Lists with `cons`
- [ ] Lists with `list`
- [ ] Lists `car`
- [ ] Lists `cdr`
- [ ] Lists `c...r`

#### Numerical functions
- [ ] Basic arithmetic operations: `+`, `-`, `*`, `/`
- [ ] Increment, decrement operators: `add1`, `sub1`
- [ ] Modulo `modulo`
- [ ] Remainder `remainder`

#### Logical and relational functions
- [ ] Comparison functions: `<?`, `<=?`, `=`, `<=` ...
- [ ] Equivalence functions: `eq?`, `eqv?`, `equal?` 
- [ ] Util functions: `null?`, `atom?`, `pair?`, `number?`
- [ ] Logical operators: `and`, `or`, `not`

#### Additional Scheme functions
- [ ] `append` for working with lists
- [ ] `delete!` for working with lists
- [ ] `even?`, `odd?` for working with numbers
- [ ] `proc?` for recognizing functions as arguments

### How to run from source?
#### Windows

#### Linux
Install [sbt](https://www.scala-sbt.org/) or by running `sudo apt install sbt`. Clone this repository, and from terminal navigate to root folder of the cloned repository. Run `sbt` from command line.

Once the interpreter is opened, type `compile` in order to build the project and type `run` in order to run. To run all tests type `test`.

### How to use?
Coming soon...

### How to contribute?
Coming soon...
