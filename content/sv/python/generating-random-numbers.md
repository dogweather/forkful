---
title:                "Generering av slumpmässiga tal"
html_title:           "Python: Generering av slumpmässiga tal"
simple_title:         "Generering av slumpmässiga tal"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är ett viktigt verktyg inom programmering och kan användas för många olika ändamål, som till exempel att skapa slumpmässiga data för tester eller simuleringar, skapa spel och lotterier eller för kryptering av data.

## Så här gör du

För att generera slumpmässiga nummer i Python finns det flera olika metoder att välja mellan. Det enklaste sättet är att använda funktionen `random` från standardbiblioteket. Här är ett exempel på hur du kan använda den för att generera ett slumpmässigt tal mellan 1 och 10:

```Python
import random

tal = random.randint(1, 10)
print(tal)
```

Detta kommer att ge ett slumpmässigt tal varje gång koden körs, eftersom det baseras på en algoritm som är beroende av tiden när funktionen anropas.

Om du vill ha mer kontroll över dina slumpmässiga nummer kan du använda `random.seed()` för att ange en startpunkt för algoritmen. Du kan också använda funktioner som `random.choice()` och `random.shuffle()` för att välja slumpmässiga element från en lista eller för att blanda om ordningen på elementen i en lista.

## På djupet

Bakom kulisserna genererar funktionen `random` slumpmässiga nummer genom att använda en pseudoslumpgenerator. Det betyder att numren är "slumpmässiga nog" för att fungera för de flesta användningsområden, men de är egentligen inte helt slumpmässiga. Det finns vissa algoritmer som kan förutsäga nästa nummer som kommer att genereras, men för de flesta applikationer är detta inte ett problem.

Det finns också andra sätt att generera slumpmässiga nummer i Python, som att använda tredjepartsbibliotek som `numpy` eller `scipy`, som har mer avancerade funktioner för att skapa slumpmässiga datamängder.

## Se även

- [Dokumentation för Python's `random`-modul](https://docs.python.org/3/library/random.html)
- [Artikel om pseudoslumpgeneratorer](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [Dokumentation för `numpy.random`-modulen](https://numpy.org/doc/stable/reference/random/index.html)