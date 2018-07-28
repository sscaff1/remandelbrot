open Reprocessing;

type state = {iterations: int};

let setup = env => {
  Env.size(~width=500, ~height=500, env);
  {iterations: 10};
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

let draw = ({iterations} as state, env) => {
  let panX = (-2.);
  let panY = (-1.5);
  let zoom = 3.;
  let contrast = 20;
  let screenHeight = Env.height(env);
  let screenWidth = Env.width(env);
  let screenHeightf = float_of_int(screenHeight);
  let screenWidthf = float_of_int(screenWidth);
  switch (iterations) {
  | i when i < 30 =>
    for (i in 0 to screenWidth) {
      for (j in 0 to screenHeight) {
        let x = float_of_int(i) *. zoom /. screenWidthf +. panX;
        let y = float_of_int(j) *. zoom /. screenHeightf +. panY;
        let count = belongsToSet(~maxIterations=iterations, ~x, ~y, ());
        let countDiff = iterations - count;
        let colorProp = contrast * countDiff;
        let color =
          Utils.color(~r=colorProp, ~g=colorProp, ~b=colorProp, ~a=255);
        Draw.pixel(~color, ~pos=(i, j), env);
      };
    };
    drawText(iterations, env);
    {iterations: iterations + 1};
  | _ =>
    drawText(iterations, env);
    state;
  };
};

run(~setup, ~draw, ());