
#!/usr/bin/env python3

import os
import traceback
from typing import Any, Dict, List, Tuple

from helper.definitions import (AssignStm, CompoundStm, EseqExp, IdExp, NumExp,
                                OpExp, PrintStm, Stm, binop, exp)


def updateSymbols(id: str, value: Any, symbols: Dict[str, Any]) -> dict:
    if symbols is None:
        symbols = {id: value}
    else:
        symbols[id] = value

    return symbols


def lookUp(id: str, symbols: Dict[str, Any]) -> Any:
    return None if id not in symbols.keys() else symbols[id]


def doBinaryOp(x: int, op: binop, y: int):
    if op == binop.Plus:
        return x + y

    elif op == binop.Minus:
        return x - y

    elif op == binop.Times:
        return x * y

    elif op == binop.Div:
        return x/y

    else:
        raise Exception(f'Unknown binop operator: {op}')


def doPrint(items: List[exp], symbols: Dict[str, Any]) -> None:
    if items is None or len(items) == 0:
        print("")
    else:
        result, newSymbols = InterpExp(items.pop(0), symbols)
        print(f'{result} ')
        doPrint(items, newSymbols)


def InterpExp(e: exp, symbols: Dict[str, Any]) -> Tuple[Any, Dict[str, Any]]:
    if isinstance(e, IdExp):
        return lookUp(e.id, symbols), symbols

    if isinstance(e, NumExp):
        return e.num, symbols

    if isinstance(e, OpExp):
        leftResult, leftSymbols = InterpExp(e.e1, symbols)
        rightResult, rightSymbols = InterpExp(e.e2, leftSymbols)
        return doBinaryOp(leftResult, e.op, rightResult), rightSymbols

    if isinstance(e, EseqExp):
        newSymbols = InterpStm(e.stm, symbols)
        return InterpExp(e.e1, newSymbols)

    else:
        raise Exception(f'type: {e}  not implemented in InterpExp function')


def InterpStm(stm: Stm, symbols: Dict[str, Any]) -> Dict[str, Any]:
    if isinstance(stm, CompoundStm):
        newSymbols = InterpStm(stm.stm1, symbols)
        return InterpStm(stm.stm2, newSymbols)

    elif isinstance(stm, AssignStm):
        result, newSymbols = InterpExp(stm.e1, symbols)
        return updateSymbols(stm.id, result, newSymbols)

    elif isinstance(stm, PrintStm):
        doPrint(stm.items, symbols)
        return symbols

    else:
        raise Exception(f'type: {stm}  not implemented in InterpStm function')


def interp(stm: Stm):
    return InterpStm(stm, {})


def main():

    while True:

        try:
            inputStr = input('\n#input: ')
            prog = None

            if inputStr == 'exit':
                os._exit(1)

            elif inputStr == 'test':
                prog = CompoundStm(
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

            else:
                prog = eval(inputStr)

            if prog is not None:
                print(interp(prog))

        except Exception as _:
            print('An error has occurred:\n')
            print(traceback.format_exc())


if __name__ == '__main__':
    main()
