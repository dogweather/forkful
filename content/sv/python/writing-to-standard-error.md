---
title:                "Skriva till standardfel"
html_title:           "Python: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfel är ett användbart verktyg i Python-programmering när man vill skriva ut felmeddelanden eller annan viktig information som är viktig för programmet att kommunicera till användaren.

## Så här gör du

För att skriva till standardfel i Python, använd `stderr` från `sys` biblioteket. Se till att importera `sys` biblioteket först.

```Python
import sys
```

Sedan kan du använda `sys.stderr.write()` för att skriva din önskade text till standardfel.

```Python
sys.stderr.write("Detta är ett felmeddelande.\n")
```

Ett användbart exempel på detta är när du vill informera användaren om att programmet stöter på ett fel och måste avslutas.

```Python
try:
    # kod som kan stöta på fel
except:
    # skriv ut felmeddelande till standardfel
    sys.stderr.write("Ett fel uppstod, programmet måste avslutas.\n")
    # avsluta programmet
    sys.exit()
```

## Djupdykning

`sys.stderr.write()` fungerar som en vanlig `print()` funktion, men skriver direkt till standardfel istället för standardutmatning. Detta gör det möjligt för dig att separera viktig program- output från eventuella felmeddelanden som kan uppstå. Du kan också använda `sys.stderr.flush()` för att omedelbart skriva ut all data som finns i standardfel bufferten.

## Se också

- [Dokumentation för sys biblioteket](https://docs.python.org/3/library/sys.html)
- [Python felsöknings guide](https://realpython.com/python-traceback/)