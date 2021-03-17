# rot_trans_mol: Rotate and translate a molecule.

    This package provides multiple methods to rotate or translate (the later is ordinary).
    It can uniformly as well as randomly rotate a molecule in three-dimensional space.

    This program is part of a task, so a executable file with functions a little wider than
    just rotation and translation is provided. But you can just use other files except rand_rot_trans.f90.

    A quaternion method may be used to perform a rotation, which is described below:

    For a vector = (x1, y1, z1), let a quaternion q1 be (0, x1, y1, z1).

    And let the above vector rotates around axis (x0, y0, z0) by an angle of theta, where (x0, y0, z0)
    if a NORMALIZED vector, we can let a quaternion p = 
    (cos(theta), sin(theta) * x0, sin(theta) * y0, sin(theta) * z0).
    then we do q2 = p * q1 * (p*), where p* stands for the conjugation of p.
    We can prove that q2 has the form (0, x2, y2, z2), and (x2, y2, z2) is the rotated vector.

