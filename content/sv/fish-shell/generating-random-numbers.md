---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Generering av slumpmässiga tal är processen av att skapa ett nummer på ett sätt som dess värde inte kan förutsägas logiskt. Programmerare gör detta för att simulera händelser, tester eller för att skapa säkerhetskoder.

## Hur gör man:
Här är ett kodexempel för att generera ett slumpmässigt tal i Fish Shell.

```Fish Shell
# Anger ett övre gräns för det slumpmässiga talet
set -l upperLimit 100

# Genererar ett slumpmässigt tal mellan 1 och övre gräns
set -l randomNumber (math (random) % $upperLimit + 1)
echo $randomNumber
```

Exempel nummerutmat:

```Fish Shell
45
```

## Djupdykning
Historiskt sett har slumpnummergenerering använts i en mängd applikationer, från spel till säkerhetssystem. Det finns olika sätt att generera ett slumpmässigt nummer, och de skiljer sig i komplexitet och nivå på slumpmässighet.

Alternativen till `random`-kommandot i Fish Shell kan inkludera att använda externa kommandon som `jot` eller `shuf`.

Vad gäller implementeringsdetaljer använder `random`-kommandot en pseudoslumpmässig nummergenerator för att producera tal. Detta betyder att siffrorna i princip är förutsägbara, men för den genomsnittliga användaren kommer de att verka slumpmässiga.

## Se även
För mer djupgående information, kolla in följande källor:

1. [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
2. [GNU Coreutils: Shuf](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html)