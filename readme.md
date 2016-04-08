<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgheadline1">1. Boopo's Escape</a></li>
<li><a href="#orgheadline2">2. Current progress:</a></li>
<li><a href="#orgheadline3">3. <span class="todo nilTODO">TODO</span> <code>[1/3]</code></a></li>
<li><a href="#orgheadline8">4. Data</a>
<ul>
<li><a href="#orgheadline4">4.1. Pvectors</a></li>
<li><a href="#orgheadline6">4.2. Player</a>
<ul>
<li><a href="#orgheadline5">4.2.1. Notes</a></li>
</ul>
</li>
<li><a href="#orgheadline7">4.3. Turret</a></li>
</ul>
</li>
</ul>
</div>
</div>

# Boopo's Escape<a id="orgheadline1"></a>

This is a small, simple project. You fly a spaceship from one end of the screen to the other while avoiding being shot by the turret, which can be randomly placed.

# Current progress:<a id="orgheadline2"></a>

A ship and turret can be rendered on the background.

# TODO <code>[1/3]</code><a id="orgheadline3"></a>

-   [X] Determine primary data structure
-   [ ] Add the turret
-   [ ] Add turret tracking

# Data<a id="orgheadline8"></a>

## Pvectors<a id="orgheadline4"></a>

A 2-dimensional vector, `(pvector Integer Integer)` in `(pvector x y)` can represent:

-   **x:** the ship's velocity in the horizontal plane, or its horizontal distance from the origin
-   **y:** the ship's velocity in the vertical plane, or its vertical distance from the origin

## Player<a id="orgheadline6"></a>

Currently, a player is represented as a struct: `(player Pvector Natural Pvector)` in `(player v r l)`

-   **v:** A Pvector representing the ship's velocity in x- and y-coords.
-   **r:** [0,360] represents the rotation of the ship, where 0 is facing right and 90 is facing up. Later on, this should an accumulated value representing the heading of the ship's velocity.
-   **l:** A Pvector representing the location of the ship as the distance from the origin, the upper left, of the screen.

### Notes<a id="orgheadline5"></a>

The controls are still a little buggy. Left and right don't determine the *heading* of the ship's velocity, they alter the velocity itself directly. This means that pressing left and right sends you careening off in that direction, instead of just changing direction. It's kind of fun though. "Horizontal thrusters", really.

## Turret<a id="orgheadline7"></a>

The turret hasn't been implemented yet, here are some ideas:

-   The turret tracks the players location.
-   every 'R' seconds, it records the player's current location.
-   After recording three such locations, it determines a best-fit linear function between the three points (such as in a scatter-plot).
-   It then fires at a location f(x), taking into account the player's speed along.
