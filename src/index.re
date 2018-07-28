open Reprocessing;

type state = {
  iterations: int,
  zoom: float,
  contrast: int,
  screenHeight: float,
  screenWidth: float,
};

let setup = env => {
  let height = 500;
  let width = 500;
  Env.size(~width, ~height, env);
  {
    iterations: 10,
    zoom: 3.,
    contrast: 20,
    screenHeight: float_of_int(height),
    screenWidth: float_of_int(width),
  };
};

let rec belongsToSet =
        (~maxIterations, ~x, ~y, ~zx=0., ~zy=0., ~count=0, ~size=0., ()) =>
  switch (count, size) {
  | (c, s) when c < maxIterations && s <= 4. =>
    belongsToSet(
      ~maxIterations,
      ~x,
      ~y,
      ~zx=zx ** 2. -. zy ** 2. +. x,
      ~zy=2. *. zx *. zy +. y,
      ~count=c + 1,
      ~size=zx ** 2. +. zy ** 2.,
      (),
    )
  | (c, _) => c
  };
let drawText = (iterations, env) =>
  Draw.text(
    ~pos=(10, 10),
    ~body="Iterations: " ++ string_of_int(iterations),
    env,
  );

let draw =
    ({iterations, zoom, contrast, screenHeight, screenWidth} as state, env) => {
  let panX = (-2.);
  let panY = (-1.5);
  if (iterations < 30) {
    for (i in 0 to int_of_float(screenWidth)) {
      for (j in 0 to int_of_float(screenHeight)) {
        let x = float_of_int(i) *. zoom /. screenWidth +. panX;
        let y = float_of_int(j) *. zoom /. screenHeight +. panY;
        let count = belongsToSet(~maxIterations=iterations, ~x, ~y, ());
        let countDiff = iterations - count;
        let colorProp = contrast * countDiff;
        let color =
          Utils.color(~r=colorProp, ~g=colorProp, ~b=colorProp, ~a=255);
        Draw.pixel(~color, ~pos=(i, j), env);
      };
    };
    drawText(iterations, env);
    {...state, iterations: iterations + 1};
  } else {
    drawText(iterations, env);
    state;
  };
};

run(~setup, ~draw, ());