---
title:                "Generating random numbers"
date:                  2024-01-20T17:48:22.313078-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in Bash is all about creating unpredictable values. Programmers use them for stuff like security, games, simulations, or anywhere else you need a good mix-up.

## How to:

Bash has a nifty little variable called `$RANDOM`. It spits out a random integer between 0 and 32767.

```Bash
echo $RANDOM
```

Sample output might be:

```
10438
```

Need a number in a different range? Easy. To get a random number between 0 and 100, do this:

```Bash
echo $(( $RANDOM % 101 ))
```

For a range with a minimum other than 0, say 50 to 100, try this:

```Bash
echo $(( $RANDOM % 51 + 50 ))
```

## Deep Dive

The `$RANDOM` variable dates back to early versions of Bash. It's pseudorandom, meaning it uses an algorithm, so it's not truly random—fine for scripts but not for serious crypto stuff.

Alternatives? Sure. There's `shuf` for shuffling lines or numbers, `awk` for a bit more programming power, and if you're up for some extra security, go for `/dev/random` or `/dev/urandom`.

As for the nitty-gritty, `$RANDOM` gets its seeds from the process ID or time, which is why it's not cryptographically secure. For most script tasks, though, it's a champ.

## See Also

- For more about Bash's `$RANDOM`, check the manual: https://www.gnu.org/software/bash/manual/bash.html#index-RANDOM
- Dive into `shuf`: `man shuf` or https://man7.org/linux/man-pages/man1/shuf.1.html
- A look at `awk`: `man awk` or https://www.gnu.org/software/gawk/manual/
- To get your head around random numbers in computing, see: https://en.wikipedia.org/wiki/Random_number_generation