import itertools

NAME = 'name'
COLOR = 'color'
DRINK = 'drink'
HEIRLOOM = 'heirloom'
ORIGIN = 'origin'
POSITION = 'position'

axes = {
    NAME: set(('winslow', 'marcolla', 'contee', 'natsiou', 'finch')),
    COLOR: set(('red', 'green', 'purple', 'blue', 'white')),
    DRINK: set(('whiskey', 'rum', 'beer', 'absinthe', 'wine')),
    HEIRLOOM: set(('diamond', 'tin', 'pendant', 'ring', 'medal')),
    ORIGIN: set(('dunwall', 'dabokva', 'fraeport', 'karnaca', 'baleton')),
    POSITION: set((1, 2, 3, 4, 5))
}

LEN = len(axes[POSITION])

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
    @classmethod
    def _apply_to_neighbor(cls, neighbor, other, axis, value):
        other_val = other[axis]
        if other_val is not None and other_val != value:
            return neighbor.set_or_verify(axis, value)

    @classmethod
    def _apply(cls, matrix, a_axis, a, b_axis, b):
        col = matrix.get_column(a, a_axis)
        if col:
            neighbors = col.get_neighbors()
            if len(neighbors) == 1:
                neighbor = neighbors[0]
                if neighbor[b_axis] is None:
                    return neighbor.set_or_verify(b_axis, b)
            elif len(neighbors) == 2:
                n1 = cls._apply_to_neighbor(neighbors[0], neighbors[1],
                                            b_axis, b)
                n2 = cls._apply_to_neighbor(neighbors[1], neighbors[0],
                                            b_axis, b)
                return n1 or n2
        return False

class CenterConstraint:
    def apply(self, matrix):
        center = matrix.get_column(3, POSITION)
        if center:
            if center[ORIGIN] == 'dunwall':
                raise ConstraintViolationError(
                    'center position cannot be from dunwall'
                )

class PurpleBlueConstraint:
    def apply(self, matrix):
        purple = matrix.get_column('purple', COLOR)
        blue = matrix.get_column('blue', COLOR)
        if purple and blue:
            if purple[POSITION] and blue[POSITION]:
                if purple[POSITION] > blue[POSITION]:
                    raise ConstraintValidationError(
                        'purple must be left of blue'
                    )

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
    SimpleConstraint('marcolla', 'fraeport'),
    NeighborConstraint('tin', 'dabokva'),
    NeighborConstraint('medal', 'karnaca'),
    NeighborConstraint('rum', 'karnaca'),
    CenterConstraint(),
    PurpleBlueConstraint(),
]

class MatrixColumn:
    def __init__(self, matrix, index):
        self.matrix = matrix
        self.index = index

    def get_neighbors(self):
        pos = self.matrix.axes[POSITION][self.index]
        if pos is not None:
            left = self.matrix.get_column(pos - 1, POSITION)
            right = self.matrix.get_column(pos + 1, POSITION)
            return [n for n in [left, right] if n is not None]
        return []

    def set_or_verify(self, axis, value):
        curr_value = self.matrix.axes[axis][self.index]
        if curr_value is None:
            self.matrix.axes[axis][self.index] = value
            return True
        if curr_value != value:
            raise ConstraintViolationError(
                'cannot set %s to %s, as it is already %s' % (
                    axis,
                    value,
                    curr_value
                )
            )
        return False

    def __getitem__(self, axis):
        return self.matrix.axes[axis][self.index]

    def __setitem__(self, axis, value):
        self.set_or_verify(axis, value)

class Matrix:
    def __init__(self, **kwargs):
        self.axes = {}
        for kwarg in kwargs:
            self.axes[kwarg] = list(kwargs[kwarg])
        for axis in axes:
            if axis not in self.axes:
                self.axes[axis] = [None] * LEN

    def clone(self):
        return self.__class__(**self.axes)

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

names = list(axes[NAME])
matrices = []

for heirloom in itertools.permutations(axes[HEIRLOOM]):
    for position in itertools.permutations(axes[POSITION]):
        matrix = Matrix(
            name=names,
            heirloom=heirloom,
            position=position,
        )
        try:
            matrix.apply_constraints()
        except ConstraintViolationError as e:
            continue
        matrices.append(matrix)

configs = {}

for matrix in matrices:
    config = [matrix.get_column(name, NAME)[HEIRLOOM] for name in names]
    configs[tuple(config)] = True

for name in names:
    print "%10s" % name,

print
print "-" * 54

configs = configs.keys()
configs.sort()

for config in configs:
    for heirloom in config:
        print "%10s" % heirloom,
    print

print

print "%d total possibilities." % len(configs)
