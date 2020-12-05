# Building Elevator Control System (ECS)
Manually time-stepped simulation (not real-time).
Number of elevators: 1..16
## First (obvious) part of algorithm
Elevator keeps moving current direction until all travellers are droppedOff (1).

Elevator stops (2) to:
* dropOff its travellers (3) or;
* pickUp new passenger wish to travel same direction while delivering passengers already inside (4).

## Obvious states (first part)
Elevator Control System:
* "common set" of floors (and respective directions) for a passenger are waiting to be pickedUp.

Each elevator:
* elevator id;
* elevator current floor;
* elevator current direction (to keep moving current direction as described in `1`);
* set of elevator dropOff floors (to serve all passengers inside, see `3`).

## Rest of algorithm
On new pickUp request (5):
* add pickUp floor and direction to "common" ECS state (6);
* if at least one elevator should stop (see `2`) there before it will change direction then nothing to do (7);
*  else closest idle elevator starts moving and updating it's set of floor accordingly (8);
* else wait for first idle elevator (9).

Before elevator try to `become idle` (10):
* choose closest pickUp request (see `9`) no elevator going to stop there (see `2`) then add this floor to elevator's state and move towards request's floor (11).

## Additional states
In `8` and `11` we can use (update) elevator's `dropOff floor set` as initial pickUp floor, so no additional states needed.
## Run tests:
```sh
$ sbt test
```
