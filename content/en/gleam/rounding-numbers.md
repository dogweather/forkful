---
title:                "Rounding numbers"
date:                  2024-01-25T02:59:59.592424-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding numbers"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding numbers is about tweaking a value to the nearest specified place—like 2.56 to 3 if we're rounding to whole numbers. Programmers do this for simplicity or to meet certain numerical specifications, usually to avoid nuances caused by floating-point precision errors or to make the output user-friendly.

## How to:
In Gleam, rounding isn't in the standard library as of my last check, but here's how you'd typically round a float to the nearest whole number using Erlang functions directly:

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // Outputs: 3
}
```

Output:
```
3
```

Got a different precision in mind? Say, rounding to two decimal places? We need a bit of math:

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // Outputs: 2.57
}
```

Output:
```
2.57
```

## Deep Dive
Historically, rounding numbers has been crucial, especially in financial and scientific computations where precision and standards matter a ton. Without rounding, you'd get nasty long decimals everywhere, making calculations impractical and error-prone.

In the programming world, different languages offer different approaches, from built-in functions to comprehensive math libraries. Rounding might involve different rules – for example, "round half up" (the usual method) or "round half to even" (often used in financial calculations to avoid bias).

Gleam, being a young language with roots in Erlang, relies on Erlang's robust set of numerical functions. As the language grows, we might see native functions introduced, cutting down the need to call external routines.

## See Also
- Erlang's :math module for more number crunching: https://erlang.org/doc/man/math.html
- For background on why rounding can get tricky, the IEEE Floating Point Standard: https://ieeexplore.ieee.org/document/8766229
- Interested in the math behind this? Check "What Every Computer Scientist Should Know About Floating-Point Arithmetic": https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
