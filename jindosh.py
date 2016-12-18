import itertools

axes = {
    'name': set(('winslow', 'marcolla', 'contee', 'natsiou', 'finch')),
    'color': set(('red', 'green', 'purple', 'blue', 'white')),
    'drink': set(('whiskey', 'rum', 'beer', 'absinthe', 'wine')),
    'heirloom': set(('diamond', 'tin', 'pendant', 'ring', 'medal')),
    'origin': set(('dunwall', 'dabokva', 'fraeport', 'karnaca', 'baleton')),
    'position': set((1, 2, 3, 4, 5))
}

LEN = len(axes['name'])

def get_axis(value):
    for axis in axes:
        if value in axes[axis]:
            return axis
    raise ValueError(value)

class ConstraintViolationError(Exception):
    pass

class ABConstraint:
    def __init__(self, a, b):
        self.a_axis = get_axis(a)
        self.b_axis = get_axis(b)
        self.a = a
        self.b = b

    def apply(self, matrix):
        a = self._apply(matrix, self.a_axis, self.a, self.b_axis, self.b)
        b = self._apply(matrix, self.b_axis, self.b, self.a_axis, self.a)
        return a or b

class SimpleConstraint(ABConstraint):
    @staticmethod
    def _apply(matrix, a_axis, a, b_axis, b):
        col = matrix.get_column(a, a_axis)
        if col:
            if col[b_axis] is None:
                col[b_axis] = b
                return True
            elif col[b_axis] != b:
                raise ConstraintViolationError(
                    '%s is %s, so %s should be %s, but it is %s' % (
                        a_axis, a, b_axis, b, col[b_axis]
                    )
                )
        return False

class NeighborConstraint(ABConstraint):
    @staticmethod
    def _apply(matrix, a_axis, a, b_axis, b):
        # TODO
        pass

constraints = [
    SimpleConstraint('contee', 'red'),
    SimpleConstraint(1, 'natsiou'),
    SimpleConstraint(2, 'green'),
    SimpleConstraint(3, 'beer'),
    SimpleConstraint('wine', 'purple'),
    SimpleConstraint('dabokva', 'white'),
    SimpleConstraint('winslow', 'diamond'),
    SimpleConstraint('baleton', 'ring'),
    SimpleConstraint('finch', 'absinthe'),
    SimpleConstraint('dunwall', 'whiskey'),
    SimpleConstraint('marcolla', 'fraeport')
]

class MatrixColumn:
    def __init__(self, matrix, index):
        self.matrix = matrix
        self.index = index

    def __getitem__(self, axis):
        return self.matrix.axes[axis][self.index]

    def __setitem__(self, axis, value):
        self.matrix.axes[axis][self.index] = value

class Matrix:
    def __init__(self, **kwargs):
        self.axes = {}
        for kwarg in kwargs:
            self.axes[kwarg] = kwargs[kwarg]
        for axis in axes:
            if axis not in self.axes:
                self.axes[axis] = [None] * LEN

    def get_column(self, value, axis=None):
        if axis is None:
            axis = get_axis(value)
        if value in self.axes[axis]:
            return MatrixColumn(self, self.axes[axis].index(value))
        return None

    def apply_constraints(self):
        keep_going = True
        while keep_going:
            keep_going = False
            for constraint in constraints:
                if constraint.apply(self):
                    keep_going = True

    def __str__(self):
        lines = []
        for axis in axes:
            line = ["%10s" % axis]
            for val in self.axes[axis]:
                if val is None:
                    val = '??'
                line.append("%10s" % str(val))
            lines.append(' '.join(line))
        return '\n'.join(lines)

for heirloom in itertools.permutations(axes['heirloom']):
    matrix = Matrix(
        name=list(axes['name']),
        heirloom=heirloom
    )
    try:
        matrix.apply_constraints()
    except ConstraintViolationError as e:
        continue
    print matrix
    print
