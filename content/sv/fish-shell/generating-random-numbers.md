---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:49:05.821877-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga tal innebär att skapa nummer som inte har något förutsägbart mönster. Programmerare använder det för saker som säkerhetskryptering, spellogik, och testdata.

## How to:
Generera ett slumpmässigt tal i Fish:

```fish
# För ett tal mellan 0 och 255
set random_num (random)
echo $random_num
```

Exempel på resultat:

```
157
```

För att specificera ett talområde:

```fish
# För ett tal mellan 1 och 100
set random_num (random 1 100)
echo $random_num
```

Exempel på resultat:

```
42
```

## Deep Dive
I äldre programmeringsspråk var slumpmässiga tal inte alltid så slumpmässiga. De kunde vara förutsägbara, vilket var ett problem i säkerhetssammanhang. Fish använder en pseudo-slumptalsgenerator, vilket ger en bra balans mellan hastighet och oförutsägbarhet.

Alternativ inkluderar att använda externa verktyg som `openssl` eller `/dev/random` i Unix-baserade system. I Fish kan du till exempel koppla samman dem så här:

```fish
# Använder /dev/random
set random_bytes (head -c 2 /dev/urandom | od -A n -i)
echo $random_bytes
```

## See Also
- Fish's official documentation on the `random` command: [https://fishshell.com/docs/current/cmds/random.html](https://fishshell.com/docs/current/cmds/random.html)
- Linux man page for `/dev/random`: [https://man7.org/linux/man-pages/man4/random.4.html](https://man7.org/linux/man-pages/man4/random.4.html)
- Wikipedia on Random Number Generation: [https://en.wikipedia.org/wiki/Random_number_generation](https://en.wikipedia.org/wiki/Random_number_generation)