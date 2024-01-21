---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:48:35.111308-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumptal innebär att skapa nummer som inte har något uppenbart mönster. Programmerare använder det för att testa, simulera och skapa saker som spel.

## Så Här Gör Du:
För att skapa ett slumptal i Bash, använd `$RANDOM` eller kommandon som `shuf` och `awk`.

```Bash
# Ett enkelt slumptal mellan 0 och 32767
echo $RANDOM

# Slumptal mellan 1 och 100
echo $((RANDOM % 100 + 1))

# Med shuf för ett nummer mellan 1 och 100
shuf -i 1-100 -n 1

# Med awk för ett decimaltal mellan 0 och 1
awk 'BEGIN {srand(); print rand()}'
```

Exempel på output:
```
$ echo $RANDOM
21541
$ echo $((RANDOM % 100 + 1))
57
$ shuf -i 1-100 -n 1
42
$ awk 'BEGIN {srand(); print rand()}'
0.237790
```

## Fördjupning
Funktionen `$RANDOM` i Bash genererar pseudo-slumptal och är inte avsedd för kryptografiskt säkra ändamål. Historiskt har pseudo-slumptal använts sedan datorernas tidiga dagar. Alternativt kan du använda `/dev/random` eller `/dev/urandom` för hårdvarubaserade slumptal, men dessa kan vara långsammare. För bättre kontroll över slumptalsgenerering kan programspråk som Python eller JavaScript erbjuda mer avancerade bibliotek.

## Se Även
- Bash manual: https://www.gnu.org/software/bash/manual/
- Random Number Generation (Wikipedia): https://en.wikipedia.org/wiki/Random_number_generation
- Understanding /dev/random and /dev/urandom: https://www.2uo.de/myths-about-urandom/