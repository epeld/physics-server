
### Welcome
This is an experiment in creating a sort of "Physics Microservice".

You set up a world using a DOM-like interface e.g:

(:world (:box (:x 100) (:y 50) (:z 50) (:width 10))
        (:sphere (:x 100) (:y 150) (:z 50) (:radius 10)))


then start the simulation and subscribe to changes - which are delivered
to you in the same format as above but with updated coordinates.

The initial step will be to support talking to a runnin server using SLIME,
but eventually I want to integrate other ways of communicating such as HTTP or ZMQ.