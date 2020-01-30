* Point():
 * int x, y
 * double distance(Point)

* Vector(Point):
 * double speed, angle
 * void move(void)

* Circle(Point):
 * int? radius
 * bool include(Point)
 * bool touch(Circle)
 * bool outside_map()

* Boid(Cirle, Vector):
 * sdl:color color
 * void display()

* Game():
 * list Boids
 * void move_boids()
 * void display_boids()
