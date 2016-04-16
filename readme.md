<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgheadline1">1. Boopo's Escape</a></li>
<li><a href="#orgheadline2">2. Current progress:</a></li>
<li><a href="#orgheadline3">3. <span class="todo nilTODO">TODO</span> <code>[1/5]</code></a></li>
<li><a href="#orgheadline11">4. Data</a>
<ul>
<li><a href="#orgheadline4">4.1. Pvectors</a></li>
<li><a href="#orgheadline5">4.2. Quadtree</a></li>
<li><a href="#orgheadline6">4.3. Entity</a></li>
<li><a href="#orgheadline9">4.4. Player</a>
<ul>
<li><a href="#orgheadline8">4.4.1. Notes</a></li>
</ul>
</li>
<li><a href="#orgheadline10">4.5. Turret</a></li>
</ul>
</li>
<li><a href="#orgheadline12">5. Rendering</a></li>
</ul>
</div>
</div>

# Boopo's Escape<a id="orgheadline1"></a>

This is a small, simple project. You fly a spaceship from one end of the screen to the other while avoiding being shot by the turret, which can be randomly placed.

# Current progress:<a id="orgheadline2"></a>

A ship and turret can be rendered on the background. You can fly the ship around the screen. You turn in increments of π/6, but this can be modified to any arbitrary radian unit.

# TODO <code>[1/5]</code><a id="orgheadline3"></a>

-   [X] Determine primary data structure.
-   [ ] Implement simple quadtree for collision-checking between entities (projectiles, turret, ship).
-   [ ] Add the turret.
-   [ ] Add turret tracking.
-   [ ] Implement three-layer rendering.

# Data<a id="orgheadline11"></a>

## Pvectors<a id="orgheadline4"></a>

A 2-dimensional vector, `(pvector Integer Integer)` in `(pvector x y)` can represent:

-   **x:** the ship's velocity in the horizontal plane, or its horizontal distance from the origin
-   **y:** the ship's velocity in the vertical plane, or its vertical distance from the origin

## Quadtree<a id="orgheadline5"></a>

`(node Posn [Listof Any] (U Empty [List Node Node Node Node]))`
in `(node coord content children)`

-   **Coord   :** A posn describing the upper-left coordinate of the node, which is a square.
-   **Content :** The content of a node is a list of any entities which may collide.
-   **Children:** A node can have either no children or four, each one of which is a node.

## Entity<a id="orgheadline6"></a>

`(entity Posn Number Number)`
in `(entity coord width height`

-   **Coord :** A posn describing center of the entity.
-   **Width :** The width dimension of the entity.
-   **Height:** The height dimension of the entity.

Right now I'm planning on having the player's struct extend the entity struct with magnitude and rotation.

## Player<a id="orgheadline9"></a>

`(player Natural Pvector Pvector Natural)` 
in `(player m v l t)`

-   **m:** A scalar number, the magnitude of the ship's pvector. This number is separated from the vector for convenience. It's used to scale the vector during movement.
-   **v:** A Pvector representing the ship's velocity in x- and y-coords around a unit circle. This vector is then scaled by **m** to give the appropriate velociy.
-   **l:** A Pvector representing the location of the ship as the distance from the origin, the upper left, of the screen.
-   **t:** The number of turns around the circle in "n" radian units. Used for rendering the ship at the appropriate angle and for rotating the ship during play.

### Notes<a id="orgheadline8"></a>

1.  The controls are fine now, but previously:

    -   The controls were a little buggy. Left and right didn't determine the *heading* of the ship's velocity, they alter the velocity itself directly. This means that pressing left and right sends you careening off in that direction, instead of just changing direction. It's kind of fun though. "Horizontal thrusters", really. Might be fun to look into this kind of movement more and polish it up for an alternative control scheme.

## Turret<a id="orgheadline10"></a>

The turret hasn't been implemented yet, here are some ideas:

-   The turret tracks the players location.
-   every 'R' seconds, it records the player's current location.
-   After recording three such locations, it determines a best-fit linear function between the three points (such as in a scatter-plot).
-   It then fires at a location f(x), taking into account the player's speed along.

# Rendering<a id="orgheadline12"></a>

I think I'll represent the game visually as a three-layered image. The bottom image is the background (land masses, some clouds, water, space, etc.). The second image contains the ships, obstacles, and turrets. The third image might have more clouds.
I'll look into parallax scrolling too, which could be scaled by the player's magnitude, or maybe even the player's angle.
