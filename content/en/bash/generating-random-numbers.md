---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:03.245166-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in Bash is about creating unpredictable numeric values for tasks like simulation, testing, or selecting random elements. Programmers do it to introduce variability in scripts or to simulate conditions that are otherwise hard to mimic programmatically.

## How to:

Bash provides several ways to generate random numbers, with `$RANDOM` being the simplest method. This internal Bash variable returns a pseudo-random integer between 0 and 32767.

```Bash
echo $RANDOM
```

Sample output might be `12345`, but it will vary with each execution.

For more control over the number range, you can manipulate `$RANDOM` using arithmetic expansion. To get a random number between 0 and 99, for example:

```Bash
echo $((RANDOM % 100))
```

To generate a random number within a specific range, say 20 to 50:

```Bash
echo $((RANDOM % 31 + 20))
```

For scenarios requiring numbers larger than 32767 or more sophisticated randomness, consider using `/dev/random` or `/dev/urandom`, which are special files generating random bytes derived from environmental noise. The following snippet reads 4 bytes from `/dev/urandom` and converts them into an unsigned integer:

```Bash
od -vAn -N4 -tu4 < /dev/urandom
```

Keep in mind, reading from `/dev/random` may block if there's insufficient entropy.

## Deep Dive

The `$RANDOM` variable is a feature of Bash, generating pseudo-random numbers based on a linear congruential generator (LCG). It's sufficient for basic randomness needs in scripts but isn't suitable for cryptographic applications due to its predictability.

Historically, `/dev/random` and `/dev/urandom` have been the go-to for more cryptographically secure randomness. `/dev/random` blocks when it runs out of entropy, making `/dev/urandom` a preferable choice for non-blocking operations, despite common myths suggesting it’s less secure.

For applications demanding higher cryptographic security, the `openssl` and `gpg` command-line tools offer more robust random number generation mechanisms. They are especially relevant for generating keys or performing operations requiring a high degree of unpredictability.

In conclusion, while Bash's `$RANDOM` provides an easy way to introduce randomness into scripts, turning to kernel-based methods or external tools offers better options when security or larger numbers are needed.

## See also

### Official Bash Documentation
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)

### Tutorials and Guides
- **Linuxize**: [Generate Random Numbers in Bash](https://linuxize.com/post/how-to-generate-random-numbers-in-bash/)
- **Cyberciti**: [BASH Shell Generate Random Numbers](https://www.cyberciti.biz/faq/bash-shell-script-generate-random-numbers/)
