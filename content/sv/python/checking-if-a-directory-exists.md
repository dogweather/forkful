---
title:                "Kontrollera om en mapp finns"
html_title:           "Python: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När vi programmerar, är det ofta nödvändigt att kontrollera om en mapp eller katalog existerar på vår dator. Detta kan vara användbart för att undvika fel och hantera filer på ett säkert sätt.

## Hur man:
För att kontrollera om en mapp existerar i Python, kan du använda funktionen `os.path.exists()` som är en del av det inbyggda biblioteket `os`. Nedan är ett exempel på hur du kan använda denna funktion:

```Python
import os

if os.path.exists("mapp"):
    print("Mappen finns!")
else:
    print("Mappen finns inte.")
```

Om mappen faktiskt existerar kommer `Mappen finns!` att skrivas ut i terminalen. Annars kommer `Mappen finns inte.` att skrivas ut.

## Djupdykning:
- Det finns ett antal olika sätt att kontrollera om en mapp existerar i Python, men `os.path.exists()` är den mest pålitliga metoden.
- Om du behöver skapa en ny mapp om det inte finns, kan du använda `os.makedirs()` istället för `os.path.exists()`.
- Det är också möjligt att använda `try-except` block för att hantera eventuella fel som kan uppstå när du försöker kontrollera om en mapp existerar.

## Se även:
- [Python doc - `os.path.exists()`](https://docs.python.org/3/library/os.path.html#os.path.exists)
- [RealPython - Checking Path Existence in Python](https://realpython.com/python-pathlib/#checking-path-existence)
- [GeeksforGeeks - Check if directory exists in Python](https://www.geeksforgeeks.org/python-check-if-directory-exists/)