---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:48:45.117276-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Co to i po co? Generowanie losowych liczb to podstawa wielu skryptów oraz aplikacji. Potrzebujemy tego do testowania, bezpieczeństwa i wszędzie tam, gdzie wyniki nie mogą być przewidywalne.

## How to:
```Bash
# Basic random number between 0 and 32767
echo $RANDOM

# Random number in a given range (for example: 1-100)
echo $(( 1 + $RANDOM % 100 ))

# Random number for a dice roll (1-6)
echo $(( 1 + $RANDOM % 6 ))

# Save to a variable
myNumber=$(( 1 + $RANDOM % 100 ))
echo $myNumber
```
Przykładowe wyjście:
```
17325
23
4
57
```

## Deep Dive
Historia i alternatywy: `$RANDOM` jest wbudowaną zmienną Bash, istniejącą od wersji 2. RANDOM generuje pseudolosowe liczby, co oznacza, że potrzebują one deterministycznego źródła (jak ziarno, czyli "seed") do startu. 

Dla prawdziwiej losowości, można wykorzystać `/dev/urandom` lub `/dev/random` w Linuksie, które zbierają entropię z różnych źródeł w systemie. 

Implementacja:
- `$RANDOM` korzysta z liniowego generatora kongruentnego, a jego ziarno zmienia się za każdym wywołaniem. Nie jest to wystarczające dla zastosowań kryptograficznych.
- Użycie `$RANDOM` jest szybkie i proste, ale dla większych zakresów czy wymagających zadań lepiej szukać innych narzędzi, jak `shuf` lub `openssl rand`.

## See Also
- Man Bash: [https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Bash-Variables](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Bash-Variables)
- Advanced Bash-Scripting Guide: [https://tldp.org/LDP/abs/html/randomvar.html](https://tldp.org/LDP/abs/html/randomvar.html)
- `shuf` manual: [https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html)
- `openssl rand` documentation: [https://www.openssl.org/docs/manmaster/man1/openssl-rand.html](https://www.openssl.org/docs/manmaster/man1/openssl-rand.html)
