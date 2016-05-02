<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgheadline1">1. Benchmarking</a></li>
<li><a href="#orgheadline8">2. Boopo</a>
<ul>
<li><a href="#orgheadline2">2.1. Update-Game</a></li>
<li><a href="#orgheadline3">2.2. Render-Game</a></li>
<li><a href="#orgheadline4">2.3. Render-Ship</a></li>
<li><a href="#orgheadline5">2.4. Render-BG</a></li>
<li><a href="#orgheadline6">2.5. Render-Info</a></li>
<li><a href="#orgheadline7">2.6. Render-UI</a></li>
</ul>
</li>
<li><a href="#orgheadline9">3. Quadtree</a></li>
</ul>
</div>
</div>

# Benchmarking<a id="orgheadline1"></a>

The game ticks 28 times per second. 5 seconds is 140 ticks, 30 seconds is 840 ticks.

# Boopo<a id="orgheadline8"></a>

## Update-Game<a id="orgheadline2"></a>

(time (for ([i (in-range 1000000)]) (update-game (game ex-p (entity (posn 420 420) 15 15)
ROOT))))

-   **CPU :** 3969
-   **Real:** 3975
-   **GC  :** 79

That's a ~70% decrease in speed compared to simply inserting the node in the quadtree
module. Where is it coming from?

## Render-Game<a id="orgheadline3"></a>

30 seconds

-   **CPU :** 110, 125, 109
-   **Real:** 111, 127, 135
-   **GC  :** 0, 0, 0

With overlay/align + render-info removed:

-   **CPU :** 15, 16, 32
-   **Real:** 17, 17, 19
-   **GC  :** 0, 0, 0

## Render-Ship<a id="orgheadline4"></a>

30 seconds

-   **CPU :** 16, 15, 0
-   **Real:** 19, 15, 0
-   **GC  :** 0,  0, 0

## Render-BG<a id="orgheadline5"></a>

30 seconds

-   **CPU :** 218, 250, 266
-   **Real:** 241, 256, 268
-   **GC  :** 0, 0, 0

This seems to be a major bottle neck. However, the time changes every time I run it.
That's odd, b/c I'm running it on the exact same data (ex-p and BACKG).

Without overlay/align and render-info:

-   **CPU :** 0, 0, 15
-   **Real:** 1, 2, 3
-   **GC  :** 0, 0, 0

## Render-Info<a id="orgheadline6"></a>

30 seconds

-   **CPU :** 219, 234, 234
-   **Real:** 221, 244, 244
-   **GC  :** 0, 0, 0

## Render-UI<a id="orgheadline7"></a>

-   **CPU:** 15, 31, 32
-   **Real:** 17, 23, 38
-   **GC:** 0, 0, 0

# Quadtree<a id="orgheadline9"></a>

(time (for ([i (in-range 1000000)]) (insert-node ROOT (entity (posn 360 360) 43 69))))
cpu :: 1156 real :: 1160 gc :: 31

After swapping to vector-based data structure:
(time (for ([i (in-range 1000000)]) (insert-node ROOT (entity (posn 360 360) 43 69))))
cpu time: 907 real time: 917 gc time: 32

Being able to take advantage of vectors being inherently indexed so that I don't have to
manually iterate and update a counter for the index of the screen makes this a bit faster
and easier. Unfortunately, there doesn't seem to be a functional vector-updater like there
is for lists, since vectors are normally used for mutable data. However, the quadtree
function relies on functions returning modified versions of the children nodes, so at one
point I convert the vector to a list, update it, then convert it to a vector again.

After removing vector->list and list->vector and using vector-update:
(time (for ([i (in-range 1000000)]) (insert-node ROOT (entity (posn 360 360) 43 69))))
cpu time: 813 real time: 807 gc time: 0
