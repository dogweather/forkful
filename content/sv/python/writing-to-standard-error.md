---
title:    "Python: Skrivande till standardfel"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför?

Att skriva till standard error i Python kan vara en användbar felsökningsmetod som hjälper dig att hitta och åtgärda eventuella fel i din kod. Genom att skicka ut felmeddelanden till standard error kan du lättare identifiera var problemet uppstår och vad som eventuellt orsakar det.

## Hur man skriver till standard error i Python

För att skriva till standard error i Python kan du använda funktionen `sys.stderr.write()`. Här är ett enkelt exempel:

```Python
import sys

sys.stderr.write("Det här är ett felmeddelande\n")
```

Output: `Det här är ett felmeddelande` kommer att skrivas ut i konsolen som standard error med röd text. Detta hjälper till att skapa en tydlig separation mellan utdata från programmet och eventuella felmeddelanden.

## Djupdykning

För att förstå mer om vad som händer när du skriver till standard error i Python, är det viktigt att förstå skillnaden mellan standard error och standard output. Standard error är en av tre standardströmmar som används i konsolen - de andra två är standard input och standard output. Standard error är avsedd att skicka ut felmeddelanden och andra felrelaterade utskrifter, medan standard output används för normal utdata från programmet.

När du skriver till standard error i Python, använder du funktionen `sys.stderr.write()` för att skriva till standard error-strömmen, medan `print()` funktionen skriver till standard output-strömmen. Detta innebär att om du vill skriva ut ett felmeddelande, är det bästa sättet att göra det genom att använda `sys.stderr.write()`.

## Se också

* [Dokumentation om `sys.stderr.write()`](https://docs.python.org/3/library/sys.html#sys.stderr)
* [Skillnaden mellan standard error och standard output](https://www.geeksforgeeks.org/what-is-the-difference-between-stderr-and-stdout/) 
* [Felsökning i Python med standard error](https://realpython.com/python-logging/)