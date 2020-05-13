# Todo Haskell Backend API

This git repository shows a method for laying out a Haskell web application based on how we do things at AlasConnect. We had some core goals in mind, and it took us a few years to get past early bad design decisions and derive this solution.

- Simplicity is king, but use some of the fancier stuff if it makes your life easier (i.e. lens turns out to be a great abstraction for manipulating Haskell records)
- We should be able to run any API endpoint in an arbitrary monad, rather than restricted to a single global `AppT`
- The business logic needs to be fully testable without having to do end-to-end integration tests (important!)

# "Bad" Approaches

*Put everything in IO and call functions directly from the next layer up.*

Why? The only way to properly (easily?) test this is full end-to-end integration tests.

*Make an abstract framework.*

Why? We made something similar to the [three-layer](https://github.com/Holmusk/three-layer) framework, but perhaps worse due to our initial lack of experience at the time. In the beginning it helped because it made choices for our developers they weren't prepared to make at the time, and allowed them to focus on the business functionality. In the long term it's become harder to refactor and layer in new concepts due to how rigid it ended up being.

It turns out creating a simple base layer is easier to extend when your next application zigs instead of zags. Want to swap your Servant API layer out with Scotty for some reason? Great! You don't have to fight with a prescribed framework to make that a reality.
