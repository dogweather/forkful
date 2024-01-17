---
title:                "Läsning av kommandoradsargument"
html_title:           "Python: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa kommandoradsargument är en viktig del av Python-programmering. Det innebär att läsa information som ges till ditt program via kommandoraden när du kör det. Det kan vara användbart när du vill göra dina program mer interaktiva och flexibla.

## Hur man:

För att läsa kommandoradsargument i Python använder du sys.argv-funktionen. Det är en lista som innehåller de argument som angavs när programmet startades. Här är ett enkelt exempel:

```Python
import sys
args = sys.argv
print("Kommandoradsargument: ", args)
```

Om du kör detta program med följande kommandoradsargument:

`python kommando.py argument1 argument2`

Kommer output att bli:

```Python
Kommandoradsargument: ['kommando.py', 'argument1', 'argument2']
```

## Djupdykning:

Kommandoradsargument är en funktion som funnits länge i programmering. Det är ett sätt att ge flexibilitet till program genom att tillåta användaren att ange olika värden för argument vid varje körning. Det finns också alternativ till att använda sys.argv, som argparse och getopt, som ger mer robusta sätt att läsa och använda kommandoradsargument.

För implementationen är det viktigt att komma ihåg att sys.argv returnerar en lista som startar med namnet på det körbara programmet och därefter följer de argument som angavs. Det kan också finnas behov av att omvandla argumenten till andra typer, som int eller float, beroende på hur de ska användas i programmet.

## Se även:

Här är några användbara länkar för att lära dig mer om att läsa kommandoradsargument i Python:

- [Python sys.argv documentation](https://docs.python.org/3/library/sys.html)
- [Command Line Arguments in Python](https://www.pythonforbeginners.com/system/python-sys-argv)
- [Command Line Arguments in Python with argparse](https://realpython.com/command-line-interfaces-python-argparse/)
- [Reading Command-Line Arguments in Python](https://www.geeksforgeeks.org/reading-command-line-arguments-in-python/)