---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Utskrift av felsökningsinformation (debug output) är när programmerare visar data i konsolen för att spåra programmets beteende. Det hjälper programmerare att identifiera och åtgärda buggar mer effektivt.

## Hur man gör:

Python erbjuder en inbyggd funktion för detta, `print()`. Här är hur du kan använda den:

```Python
def greeting(name):
    print(f"Debug: name = {name}")
    return f"Hej, {name}!"

# Testkörning:
print(greeting("Sven"))
```

När du kör ovanstående kod kan du se både debug output och det slutliga resultatet:

```Python
Debug: name = Sven
Hej, Sven!
```

## Djupdykning

Trots dess grundläggande användning har `print()` en intressant historia. På förhistorisk tid av programmering, var att skriva till konsolen en av de få metoder för felsökning. I Python, `print()` förändrades över tiden för att omfatta fler användningsfall.

Det finns alternativ till `print()` för felsökning i Python. Ett populärt val är Python's inbyggda `logging` modul. I jämförelse med `print()`, tillhandahåller `logging` ett mer robust verktyg för att hantera felsökningsinformation.

```Python
import logging

def hello(name):
    logging.debug(f"Debug: name = {name}")
    return f"Hej, {name}!"

logging.basicConfig(level=logging.DEBUG)

# Testkörning:
print(hello("Anna"))
```

Silent detaljer för implementationen av `print()` kan hittas i Python's CPython källkod. Det är fascinerande att ta reda på hur denna enkla funktion har optimerats för effektivitet.

## Se Även

Här är några skäl till att du kanske vill fördjupa sig mer:

1. Python's Officiella Dokumentation om `print()`: https://docs.python.org/3/library/functions.html#print
2. Python's Officiella Dokumentation om `logging`: https://docs.python.org/3/library/logging.html
3. Dykning i Python's CPython källkod: https://github.com/python/cpython