---
title:                "Generering av slumpmässiga nummer"
html_title:           "Bash: Generering av slumpmässiga nummer"
simple_title:         "Generering av slumpmässiga nummer"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
Du kanske undrar varför man skulle vilja använda slumpmässiga nummer i sitt Bash-program. En anledning kan vara för att skapa variation i ett spel eller för att generera slumpmässiga lösenord.

## Hur man gör det
För att generera slumpmässiga nummer i Bash kan man använda kommandot "shuf". Det här kommandot är en del av GNU Core Utilities och finns förinstallerat på de flesta Linux-distributioner. Här är ett exempel på hur man kan använda det:

```Bash
# Genererar ett slumpmässigt heltal mellan 1 och 10
shuf -i 1-10 -n 1

# Genererar ett slumpmässigt lösenord med 8 tecken från bokstäver, siffror och specialtecken
shuf -zer -n 8 -e {A..Z} {a..z} {0..9} !"#$%&'()*+,-./:;<=>?@[\]^_{|}~
```

Kommandot "shuf" tar in flera argument som styr hur de slumpmässiga numrerna ska genereras. I det första exemplet använder vi argumenten "-i 1-10" för att ange att vi vill generera ett tal mellan 1 och 10, och "-n 1" för att endast få en output. I det andra exemplet använder vi argumentet "-n 8" för att få en output med 8 tecken och "-e" för att specificera vilka tecken som ska ingå i lösenordet.

## Djupgående
Det finns flera sätt att generera slumpmässiga nummer i Bash. Förutom "shuf" kan man också använda kommandot "od" eller "jot". Det går även att använda "$RANDOM" variabeln för att få ett slumpmässigt heltal mellan 0 och 32767. Genom att kombinera dessa olika tekniker kan man skapa mer avancerade sätt att generera slumpmässiga nummer, till exempel genom att välja ut slumpmässiga delar av ett ord eller en fras.

## Se även
- GNU Core Utilities man-sida för "shuf": https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html
- "od" man-sida: https://www.gnu.org/software/coreutils/manual/html_node/od-invocation.html
- "jot" man-sida: https://www.freebsd.org/cgi/man.cgi?query=jot&sektion=1