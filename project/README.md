# CSCI 3250/5250 - Compilers | Semester Project

```txt
This course project is a demonstration of our course concepts to a novel situation, or
implementation of a compiler concept not previously assigned in the course. 
The frontend components of a compiler are useful in many situations that involve processing or
interpreting written text. Algorithms or concepts involved with the middle and back-end
of the compiler are useful in optimization or when satisfying certain constraints.

Your project does not need to be large or extensive, but pick a goal that seems reasonable.
Feel free to ask the instructor if you are unsure whether a given project is suitable or
not.
```

## Straight-line interpreter
A simple straight line interpreter implemented in python

### Usage

To run a test trial, run the following command and enter "test" as the input
```bash
$ python3 sli.py
#input: test
```

The following test will be run:
```txt
CompoundStm(
            AssignStm("a",
                        OpExp(NumExp(5), binop.Plus, NumExp(3))),
            CompoundStm(
                AssignStm("b",
                            EseqExp(PrintStm([IdExp("a"),
                                            OpExp(IdExp("a"), binop.Minus, NumExp(1))]),
                                    OpExp(NumExp(10), binop.Times, IdExp("a")))),
                PrintStm([IdExp("b")])
            )
        )

```

To exit, enter "exit" as the input
```python
$ python3 sli.py
$ #input: exit
```

To run a new SLI command, run the following:
```bash
$ python3 sli.py
#input: <cmd>
```

Some sample commands include:
```python
PrintStm([OpExp(NumExp(5), binop.Plus, NumExp(3))])
PrintStm([IdExp("b")])
```
