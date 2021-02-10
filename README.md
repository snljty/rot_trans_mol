# rot_trans_mol: Rotate and translate a molecule.

    The rotation is implemented using a quaternion. For details about quaternion, please use Wikipedia or so on.

    With this algorithm, one can have a random rotation on a molecule within 3 free-degrees.
    This algorithm is proved to be uniformly distributed.

    For a vector = (x1, y1, z1), let a quaternion q1 be (0, x1, y1, z1).

    And let the above vector rotates around axis (x0, y0, z0) by an angle of theta, where (x0, y0, z0)
    if a NORMALIZED vector, we can let a quaternion p = 
    (cos(theta), sin(theta) * x0, sin(theta) * y0, sin(theta) * z0).
    then we do q2 = p * q1 * (p*), where p* stands for the conjugation of p.
    We can prove that q2 has the form (0, x2, y2, z2), and (x2, y2, z2) is the rotated vector.

