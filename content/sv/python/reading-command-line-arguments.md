---
title:                "Python: Läsa kommandoradsargument"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att kunna läsa in kommandoradsargument är en viktig färdighet för varje programmerare. Det ger dig möjlighet att interagera med ditt Python-program på ett dynamiskt sätt genom att ange olika värden och inställningar från din terminal.

## Hur man gör

Att läsa in kommandoradsargument i Python är enkelt och kan åstadkommas med hjälp av argparse-modulen. För att använda denna modul, se till att du först importerar den i ditt Python-program.

```Python
import argparse
```

Sedan behöver du definiera de argument som du vill kunna läsa in. Detta görs med hjälp av ArgumentParser-objektet och dess add_argument() -metod. Till exempel, om du vill läsa in ett tal och en sträng från kommandoraden så skulle du skriva:

```Python
parser = argparse.ArgumentParser()
parser.add_argument("--numeric", type=int, help="Enter a number.")
parser.add_argument("--string", type=str, help="Enter a string.")
```

För att faktiskt läsa in dina argument, kan du sedan använda parse_args() -metoden på ditt ArgumentParser-objekt och tilldela resultaten till en variabel. Till exempel:

```Python
args = parser.parse_args()
```

Nu kan du använda de inmatade värdena i ditt program genom att åtkomsta dem med hjälp av deras respektive argumentnamn. Till exempel:

```Python
print("Nummer: {}".format(args.numeric))
print("Sträng: {}".format(args.string))
```

Om du kör det här programmet från din terminal och anger ett tal och en sträng som argument, kommer du att se följande utmatning:

```console
$ python program.py --numeric 10 --string "Hello!"
Nummer: 10
Sträng: Hello!
```

## Djupdykning

Det finns många användbara funktioner inbyggda i argparse-modulen för att läsa in kommandoradsargument. Till exempel kan du använda required = True parameter för att tvinga användaren att ange ett visst argument. Du kan också använda action = "store_true" parameter för att skapa en sträng som är "True" om flaggan anges och "False" om den inte anges. Dessutom kan du lägga till hjälpbeskrivningar till dina argument för att ge användaren mer vägledning om hur de ska användas.

## Se även

- [Python dokumentation för argparse](https://docs.python.org/3/library/argparse.html)
- [Real Python-artikel om att hantera kommandoradsargument i Python](https://realpython.com/command-line-interfaces-python-argparse/#other-argument-features)