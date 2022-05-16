
from enum import Enum
from typing import Any, List


class binop(Enum):
    Plus = 'Plus'
    Minus = 'Minus'
    Times = 'Times'
    Div = 'Div'


class exp(object):
    def __init__(self) -> None:
        pass


class IdExp(exp):
    def __init__(self, id: str) -> None:
        super(IdExp, self).__init__()
        self.id: str = id


class NumExp(exp):
    def __init__(self, num: int) -> None:
        super(NumExp, self).__init__()
        self.num: int = num


class OpExp(exp):
    def __init__(self, e1: exp, op: binop, e2: exp) -> None:
        super(OpExp, self).__init__()
        self.e1: exp = e1
        self.op: binop = op
        self.e2: exp = e2


class EseqExp(exp):
    def __init__(self, stm: Any, e1: exp) -> None:
        super(EseqExp, self).__init__()
        self.stm: Stm = stm
        self.e1: exp = e1


class Stm(object):
    def __init__(self) -> None:
        pass


class CompoundStm(Stm):
    def __init__(self, stm1: Stm, stm2: Stm) -> None:
        super(CompoundStm, self).__init__()
        self.stm1: Stm = stm1
        self.stm2: Stm = stm2


class AssignStm(Stm):
    def __init__(self, id: str, e1: exp) -> None:
        super(AssignStm, self).__init__()
        self.id: str = id
        self.e1: exp = e1


class PrintStm(Stm):
    def __init__(self, items: List[exp]) -> None:
        super(PrintStm, self).__init__()
        self.items = items
