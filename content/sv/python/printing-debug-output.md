---
title:                "Python: Utskrift av felsökningsspår"
simple_title:         "Utskrift av felsökningsspår"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/printing-debug-output.md"
---

{{< edit_this_page >}}

# Varför
Att skriva ut felsökningsutdata, eller "debug output", är en viktig del av programmering. Det kan hjälpa dig att förstå exakt vad som händer i ditt program och identifiera eventuella fel eller problem.

## Hur man gör
Att skriva ut felsökningsutdata i Python är enkelt. Du kan använda funktionen "print()" för att skriva ut olika variabler eller meddelanden. Här är ett exempel på koden som skriver ut texten "Hej världen!" till konsolen:

```Python
print("Hej världen!")
```

Detta kommer att ge följande utdata:
```
Hej världen!
```

Du kan också använda "format()" funktionen för att formatera utdatan på ett mer läsbart sätt. Här är ett exempel på hur man skriver ut en text med variabler i Python:

```Python
namn = "Anna"
ålder = 25
print("Hej, mitt namn är {} och jag är {} år gammal.".format(namn, ålder))
```

Detta kommer att ge följande utdata:
```
Hej, mitt namn är Anna och jag är 25 år gammal.
```

## Djupdykning
När man skriver ut felsökningsutdata är det viktigt att tänka på vad som är relevant att skriva ut. Om du har många variabler i ditt program så kan det vara en god idé att bara skriva ut de viktigaste för att hålla konsolen ren och lättläst.

Du kan också använda villkorsuttryck för att endast skriva ut utdata när ett visst villkor är uppfyllt. Här är ett exempel på hur man bara skriver ut utdata om ett nummer är större än 10:

```Python
num = 15
if num > 10:
    print("{} är större än 10".format(num))
```

Detta kommer att ge följande utdata:
```
15 är större än 10
```

En annan användbar funktion är "logging" som kan hjälpa dig att ordna och filtrera felsökningsutdata. Du kan också använda "try-except" block för att fånga och skriva ut eventuella felmeddelanden.

# Se även
- [Python felsökning](https://www.python.org/doc/glossary.html#term-debugging)
- [Python print() funktionen](https://docs.python.org/3/library/functions.html#print)
- [Python format() funktionen](https://docs.python.org/3/library/stdtypes.html#str.format)