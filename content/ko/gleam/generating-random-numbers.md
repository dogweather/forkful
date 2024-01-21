---
title:                "난수 생성하기"
date:                  2024-01-20T17:48:52.994219-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
Random numbers are unpredictable values. Programmers use them for things like games, simulations, and security applications.

## How to: (어떻게:)
```gleam
import gleam/rand

pub fn main() {
  let random_number = rand.int(1, 100) // Generates a random number between 1 and 100
  println("Your random number is: ", int_to_string(random_number))
}
```
Sample output:
```
Your random number is: 42
```

## Deep Dive (심층 분석)
Generating random numbers has been pivotal in computing for decades. Early computers used various hardware-based methods. Gleam, being built on the Erlang ecosystem, benefits from the robustness of Erlang's randomness functions. Alternatives exist, like pseudo-random number generators (PRNGs), but they aren't truly random – they're deterministic. Gleam's `rand` module provides an interface to Erlang's secure randomness sources, which is essential for applications needing cryptographic safety.

## See Also (관련 정보)
- [Erlang's :rand module information](http://erlang.org/doc/man/rand.html)
- [Understanding randomness in computers](https://www.random.org/randomness/)