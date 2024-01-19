---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa kommandoradsargument i Python innebär att hämta datan som skickas till ett program vid utförandetid. Detta är viktigt för att tillåta dynamisk programhantering baserat på användarens input.

## Hur man gör:

För att hämta kommandoradsargument i Python, använder vi `sys`-modulen. Här är en enkel kod: 

```Python
import sys

for arg in sys.argv:
    print(arg)
```

Om du kör programmet med `python my_script.py arg1 arg2 arg3`, kommer output att bli: 

```Python
my_script.py
arg1
arg2
arg3
```

'## Djupdykning

Funktionen `sys.argv` är inget nytt; det har förekommit sedan början av Unix-stilen på kommandoradsoperativsystem. 

Alternativ inkluderar att använda `argparse`-modulen, som ger programmeraren mer flexibilitet och kontroll men kräver mer kodning. 

Ett intressant detalj om `sys.argv` är att den returnerar en lista, där index `0` är skriptets namn och resten är argumenten i ordningen de skickas till programmet.

## Se även

För en djupare förståelse, överväg dessa resurser: 

- Python's officiella dokumentation på `sys`: https://docs.python.org/3/library/sys.html
- en utförlig guide om `argparse`: https://docs.python.org/3/library/argparse.html
- ett fascinerande inlägg om Unix-kommandoradens historia: http://www.linusakesson.net/programming/tty/index.php