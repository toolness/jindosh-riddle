'''
    This solution to the Jindosh Riddle works in Python 2.7 or 3.x.
'''

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

def get_axis(value):
    '''
    Given a value, return the axis it belongs to.
    '''

    for axis in axes:
        if value in axes[axis]:
            return axis
    raise ValueError(value)

class ConstraintViolationError(Exception):
    '''
    This exception is thrown by a Constraint whenever the riddle
    solution matrix it's applied against violates it.
    '''

    pass

class Constraint:
    '''
    A Constraint is an object with a single method, `apply()`, that
    takes a potential solution Matrix and applies a constraint against
    it.

    Constraints can optionally make alterations to proposed solutions;
    this represents "deductions" that the solver is making about the
    unknown properties of a proposed solution.

    The `apply()` method should raise a ConstraintViolationError if
    the proposed solution violates the constraint. Otherwise, it should
    return True if the solution was modified (mutated) based on the
    constraint, or False if not.
    '''

    def apply(self, matrix):
        raise NotImplementedError()

class ABConstraint(Constraint):
    '''
    A bi-directional constraint consisting of two properties and values.

    This is an abstract class.
    '''

    def __init__(self, a, b):
        self.a_axis = get_axis(a)
        self.b_axis = get_axis(b)
        self.a = a
        self.b = b

    def _apply(self, matrix, a_axis, a, b_axis, b):
        '''
        Apply the constraint with the given parameters on the given Matrix,
        returning True if the Matrix was mutated and False otherwise.
        '''

        raise NotImplementedError()

    def apply(self, matrix):
        a = self._apply(matrix, self.a_axis, self.a, self.b_axis, self.b)
        b = self._apply(matrix, self.b_axis, self.b, self.a_axis, self.a)
        return a or b

class SimpleConstraint(ABConstraint):
    '''
    A constraint stating that a person with property `a` along
    `a_axis` must also have property `b` along `b_axis`.
    '''

    @staticmethod
    def _apply(matrix, a_axis, a, b_axis, b):
        col = matrix.get_column(a, a_axis)
        if col:
            return col.set_or_verify(b_axis, b)
        return False

class NeighborConstraint(ABConstraint):
    '''
    A constraint stating that a person with property `a` along
    `a_axis` must have a neighbor with property `b` along `b_axis`.
    '''

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

class PurpleIsLeftOfBlueConstraint(Constraint):
    '''
    A constraint stating that the person wearing purple must be
    immediately to the left of the person wearing blue.
    '''

    def apply(self, matrix):
        purple = matrix.get_column('purple', COLOR)
        blue = matrix.get_column('blue', COLOR)
        if purple and blue:
            if purple[POSITION] and blue[POSITION]:
                # The riddle is ambiguous about whether the purple
                # person must be *immediately* to the left of the 
                # blue person, or just "to the left of" in general,
                # but in order to obtain one unique result, we
                # need to interpret it as *immediately*.
                if purple[POSITION] != blue[POSITION] - 1:
                    raise ConstraintViolationError(
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
    PurpleIsLeftOfBlueConstraint(),
]

class MatrixColumn:
    '''
    A column of a Matrix, representing a particular person in a
    potential solution for the Jindosh riddle.
    '''

    def __init__(self, matrix, index):
        self.matrix = matrix
        self.index = index

    def get_neighbors(self):
        '''
        Return any MatrixColumns corresponding to the known neighbors of
        this one.
        '''

        pos = self.matrix.axes[POSITION][self.index]
        if pos is not None:
            left = self.matrix.get_column(pos - 1, POSITION)
            right = self.matrix.get_column(pos + 1, POSITION)
            return [n for n in [left, right] if n is not None]
        return []

    def set_or_verify(self, axis, value):
        '''
        Set the given property axis to the given value on the Matrix
        that this MatrixColumn comes from.

        Returns True if the Matrix was mutated, False otherwise.
        '''

        curr_value = self.matrix.axes[axis][self.index]
        if curr_value is None:
            if self.matrix.get_column(value, axis) is not None:
                raise ConstraintViolationError(
                    'cannot set %s to %s, as it is '
                    'already present elsewhere' % (
                        axis,
                        value
                    )
                )
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
    '''
    Represents one potential solution to the Jindosh Riddle. Each
    column represents a particular individual across all axes of
    their persona (name, drink, heirloom, etc).

    Each cell in the Matrix is `None` if its value is not yet known.
    '''

    def __init__(self, **kwargs):
        self.axes = {}
        for kwarg in kwargs:
            self.axes[kwarg] = list(kwargs[kwarg])
        for axis in axes:
            if axis not in self.axes:
                self.axes[axis] = [None] * len(axes[axis])
        self.apply_constraints()

    def get_column(self, value, axis=None):
        '''
        Return a MatrixColumn corresponding to the person with the
        given property, or None if no such person exists.
        '''

        if axis is None:
            axis = get_axis(value)
        if value in self.axes[axis]:
            return MatrixColumn(self, self.axes[axis].index(value))
        return None

    @staticmethod
    def _apply_permutation(row, unfilled_spots, permutation):
        row = row[:]
        for spot, i in zip(unfilled_spots, range(len(permutation))):
            row[spot] = permutation[i]
        return row

    def permute(self, axis):
        '''
        This generator yields Matrices with the unfilled cells along the
        given axis filled-in.

        For instance, if three of five heirlooms have been filled out, this
        will yield the two permutations of the Matrix with all the cells
        filled-in. 
        '''

        all_values = axes[axis]
        filled_values = set([
            value for value in self.axes[axis]
            if value is not None
        ])
        unfilled_spots = [
            i for i in range(len(self.axes[axis]))
            if self.axes[axis][i] is None
        ]
        values_to_permute = all_values.difference(filled_values)

        for permutation in itertools.permutations(values_to_permute):
            newaxes = self.axes.copy()
            newaxes[axis] = self._apply_permutation(
                self.axes[axis],
                unfilled_spots,
                permutation
            )
            try:
                matrix = self.__class__(**newaxes)
            except ConstraintViolationError:
                continue
            yield matrix

    def apply_constraints(self):
        '''
        Continuously apply all constraints to this particular solution
        until none of them change the solution.
        '''

        keep_going = True
        while keep_going:
            keep_going = False
            for constraint in constraints:
                if constraint.apply(self):
                    keep_going = True

    def final_sanity_check(self):
        '''
        If all the cells in the Matrix are filled out, this ensures that
        they cover all possible values on each axis, e.g. that no two
        people have the same drink/heirloom/et cetera.
        '''

        for axis in axes:
            if set(self.axes[axis]) != axes[axis]:
                raise AssertionError(
                    'solution %s is invalid: %s' % (axis, self.axes[axis])
                )

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

def solve_riddle():
    '''
    Find the Matrix corresponding to the unique solution of the
    Jindosh Riddle and return it.
    '''

    solution = None
    starting_matrix = Matrix(name=list(axes[NAME]))

    for heirloomed_matrix in starting_matrix.permute(HEIRLOOM):
        for positioned_matrix in heirloomed_matrix.permute(POSITION):
            for colored_matrix in positioned_matrix.permute(COLOR):
                for drinked_matrix in colored_matrix.permute(DRINK):
                    for final_matrix in drinked_matrix.permute(ORIGIN):
                        if solution is not None:
                            raise AssertionError('multiple solutions found')
                        solution = final_matrix

    solution.final_sanity_check()
    return solution

if __name__ == '__main__':
    print(solve_riddle())
