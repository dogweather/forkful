---
title:                "Läsa en textfil"
html_title:           "Python: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa textfiler är en grundläggande uppgift för programmerare eftersom det tillåter oss att få tillgång till och bearbeta data på ett effektivt sätt.

## Hur man gör det

För att läsa en textfil i Python, behöver du först öppna filen med hjälp av den inbyggda funktionen `open()`. Du behöver ange filnamnet och läge (t.ex. "r" för läsning) som parametrar. Sedan kan du använda en `for`-loop för att iterera över varje rad i filen och genomföra önskade operationer.

```Python
file = open("textfil.txt", "r")

for line in file:
    # vidta åtgärder för varje rad i filen
    print(line) # exempelvis att skriva ut varje rad

file.close()
```
Exempel på utdata:

```
Detta är första raden i textfilen.
Detta är andra raden.
Och detta är den tredje och sista raden.
```

## Djupdykning

När du läser en textfil i Python, returneras varje rad som en sträng. Om du vill konvertera denna sträng till ett objekt av typen `list`, kan du använda metoden `split()`.

```Python
file = open("textfil.txt", "r")

for line in file:
    words = line.split() # konvertera till list

    # vidta åtgärder för varje ord i raden
    for word in words:
        print(word) # exempelvis att skriva ut varje ord

file.close()
```
Exempel på utdata:

```
Detta
är
första
raden
Detta
är
andra
raden.
Och
detta
är
den
tredje
och
sista
raden.
```

## Se även

- Dokumentation för `open()`-funktionen: https://docs.python.org/sv/3/library/functions.html#open
- Dokumentation för `split()`-metoden: https://docs.python.org/sv/3/library/stdtypes.html#str.split