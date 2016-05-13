<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgheadline1">1. Boopo's Escape</a></li>
<li><a href="#orgheadline2">2. Current progress:</a></li>
<li><a href="#orgheadline3">3. <span class="todo nilTODO">TODO</span> <code>[4/7]</code></a></li>
<li><a href="#orgheadline16">4. Data</a>
<ul>
<li><a href="#orgheadline4">4.1. Pvectors</a></li>
<li><a href="#orgheadline8">4.2. Quadtree</a>
<ul>
<li><a href="#orgheadline5">4.2.1. Update to main module:</a></li>
<li><a href="#orgheadline6">4.2.2. Multiple insertions into a quadtree</a></li>
<li><a href="#orgheadline7">4.2.3. Rectangles vs. Square quad</a></li>
</ul>
</li>
<li><a href="#orgheadline9">4.3. Entity</a></li>
<li><a href="#orgheadline13">4.4. Player</a>
<ul>
<li><a href="#orgheadline12">4.4.1. Notes</a></li>
</ul>
</li>
<li><a href="#orgheadline14">4.5. Turret</a></li>
<li><a href="#orgheadline15">4.6. Obstacles</a></li>
</ul>
</li>
<li><a href="#orgheadline19">5. Rendering</a>
<ul>
<li><a href="#orgheadline17">5.1. Progress:</a></li>
<li><a href="#orgheadline18">5.2. Scrolling</a></li>
</ul>
</li>
</ul>
</div>
</div>

# Boopo's Escape<a id="orgheadline1"></a>

This is a small, simple project. You fly a spaceship from one end of the screen to the other while avoiding being shot by the turret, which can be randomly placed.

# Current progress:<a id="orgheadline2"></a>

A ship and turret can be rendered on the background. You can fly the ship around the screen. You turn in increments of π/6, but this can be modified to any arbitrary radian unit.

# TODO <code>[4/7]</code><a id="orgheadline3"></a>

-   [X] Determine primary data structure.
-   [X] Implement simple quadtree for collision-checking between entities (projectiles, turret, ship).
-   [X] Add the turret.
-   [X] Add turret tracking.
-   [ ] Implement three-layer rendering.
-   [ ] Map the absolute coordinates of entities on the background image to the relative coordinates of the screen and the quadtree.
-   [ ] Refactor turret tracking from a purely visual effect to a part of the turret's data structure (will have to extend turret's base entity struct with another one)

# Data<a id="orgheadline16"></a>

## Pvectors<a id="orgheadline4"></a>

A 2-dimensional vector, `(pvector Integer Integer)` in `(pvector x y)` can represent:

-   **x:** the ship's velocity in the horizontal plane, or its horizontal distance from the origin
-   **y:** the ship's velocity in the vertical plane, or its vertical distance from the origin

## Quadtree<a id="orgheadline8"></a>

`(node Posn [Listof Any] (U Empty [List Node Node Node Node]))`
in `(node coord content children)`

-   **Coord   :** A posn describing the upper-left coordinate of the node, which is a square.
-   **Content :** The content of a node is a list of any entities which may collide.
-   **Children:** A node can have either no children or four, each one of which is a node.

See quad-notes.

### Update to main module:<a id="orgheadline5"></a>

I have to change the big-bang function to update the quadtree. I can see two options here right off
the bat: We refactor the module so that the quadtree is now the defining
data structure, instead of the game struct. I'm not sure how I'll modify
the quadtree module so that it doesn't just toss away player information
(since it just uses entity, a supertype). The other option is two insert
the quadtree as an additional field to the game struct. This feels
redudant, since we'll be carrying along all the player and turret
information in both the game struct AND the quadtree.

-   **Status:** I made the quadtree a secondary data structure, it's carried along in the field of the game-struct. I'll have to look at this more closely. Right now it's VERY slow to move around the screen.
    -   Whoops! It runs super slow because it recalculates the quad-tree every tick, instead of when objects move.

### Multiple insertions into a quadtree<a id="orgheadline6"></a>

I could add support for inserting multiple items during one walk (instead of having to walk the tree once per insertion) by using a folding or accumulated function. The accumulator represents the tree so far, and the function consumes an arbitrary list of things to insert. Every node you check if any entity fits into a child-node and insert it appropriately.

### Rectangles vs. Square quad<a id="orgheadline7"></a>

Note, I'm going to have to reconfigure the quadtree module to use rectangles instead of squares. The ship is rectangle, and the obstacles on the map are also rectang

## Entity<a id="orgheadline9"></a>

`(entity Posn Number Number)`
in `(entity coord width height`

-   **Coord :** A posn describing center of the entity.
-   **Width :** The width dimension of the entity.
-   **Height:** The height dimension of the entity.

Right now I'm planning on having the player's struct extend the entity struct with magnitude and rotation.

## Player<a id="orgheadline13"></a>

`(player Natural Pvector Pvector Natural)` 
in `(player m v l t)`

-   **m:** A scalar number, the magnitude of the ship's pvector. This number is separated from the vector for convenience. It's used to scale the vector during movement.
-   **v:** A Pvector representing the ship's velocity in x- and y-coords around a unit circle. This vector is then scaled by **m** to give the appropriate velociy.
-   **l:** A Pvector representing the location of the ship as the distance from the origin, the upper left, of the screen.
-   **t:** The number of turns around the circle in "n" radian units. Used for rendering the ship at the appropriate angle and for rotating the ship during play.

### Notes<a id="orgheadline12"></a>

1.  The controls are fine now, but previously:

    -   The controls were a little buggy. Left and right didn't determine the *heading* of the ship's velocity, they alter the velocity itself directly. This means that pressing left and right sends you careening off in that direction, instead of just changing direction. It's kind of fun though. "Horizontal thrusters", really. Might be fun to look into this kind of movement more and polish it up for an alternative control scheme.

2.  I think I might have to make the player mutable. Right now the player is constantly being reinserted into the quadtree, and the quadtree constantly being rebuilt, multiple times every second. It's easier to set! the player's new location and speed, and only re-insert every time it moves to a new quadrant or new objects are introduced.

## Turret<a id="orgheadline14"></a>

The turret hasn't been implemented yet, here are some ideas:

-   The turret tracks the players location.
-   every 'R' seconds, it records the player's current location.
-   After recording three such locations, it determines a best-fit linear function between the three points (such as in a scatter-plot).
-   It then fires at a location f(x), taking into account the player's speed along.

## Obstacles<a id="orgheadline15"></a>

Right now there are no obstacles to hide behind or collide into. How would I implement obstacles?

-   Random rectangles scattered across the screen.

Rendering obstacles is going to be kind of difficult. I can't just slap them into a 720 by 720 square, because that square scrolls as the player moves. Maybe I can generate obstacles in a square surrounding the turret? The further away you are, the less cover there is.

# Rendering<a id="orgheadline19"></a>

I think I'll represent the game visually as a three-layered image. The bottom image is the background (land masses, some clouds, water, space, etc.). The second image contains the ships, obstacles, and turrets. The third image might have more clouds.
I'll look into parallax scrolling too, which could be scaled by the player's magnitude, or maybe even the player's angle.

## Progress:<a id="orgheadline17"></a>

Renders by overlaying the ship over the turret over the background.
The background is rendered by "scrolling" a larger image via successive cropping around the player.

## Scrolling<a id="orgheadline18"></a>

Takes a background image, the current coordinates of the upper-left corner, and the rate at which the background scrolls by x- and y-coords. It produces a new image, cropped to the dimensions of the screen such that the player is centered, with the background "scrolled" in the opposite direction by the player's speed.
The initial coordinates are determined by a constant, which is the coordinate location of the viewport on the background screen. As the player moves around the screen, the background is scrolled by their speed per tick.
Maybe I could stick the current coordinates of the background in the game-struct? That might make it easier to keep track of where the screen is in relation to the background, so we know when to stop scrolling.

-   **Solved:** Increase the size of the background, decrease the rate at which the background scrolls in proportion to the player's distance from the center. Now you won't be able to scroll past the boundaries of the background image at all.
