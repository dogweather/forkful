---
title:    "Python: Kontrollera om en mapp finns"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Varför

I denna bloggpost kommer vi att prata om hur man kontrollerar om en mapp existerar i Python. Detta är en viktig grundläggande färdighet som är nödvändig för att kunna hantera mappar och filer i dina Python-program. Det kan vara användbart när du vill se till att en mapp finns innan du försöker skapa en ny fil i den, eller när du vill lista alla filer som finns i en viss mapp. Det är ett viktigt verktyg att ha i din programmeringsverktygslåda.

## Hur man gör

För att kontrollera om en mapp existerar i Python, kan du använda funktionen `os.path.exists()`. Detta är en inbyggd funktion i Python som tar en sökväg till en fil eller en mapp som argument. Om sökvägen existerar kommer funktionen att returnera `True`, annars returneras `False`.

```python
import os

# Ange sökvägen till mappen som du vill kontrollera
sökväg = "/hem/användare/mapp"

# Kontrollera om sökvägen existerar
if os.path.exists(sökväg):
    print("Mappen existerar!")
else:
    print("Mappen existerar inte.")
```

Om mappen existerar kommer du att se följande utmatning:

```
Mappen existerar!
```

Om mappen inte existerar kommer du istället att se:

```
Mappen existerar inte.
```

Du kan också använda funktionen `os.path.isdir()` för att kontrollera om sökvägen pekar på en mapp eller en fil. Om sökvägen pekar på en mapp kommer funktionen att returnera `True`, annars returneras `False`.

```python
import os

# Ange sökvägen till mappen som du vill kontrollera
sökväg = "/hem/användare/mapp"

# Kontrollera om sökvägen pekar på en mapp
if os.path.isdir(sökväg):
    print("Sökvägen pekar på en mapp!")
else:
    print("Sökvägen pekar inte på en mapp.")
```

## Djupdykning

Nu när vi har sett hur man använder `os.path.exists()` och `os.path.isdir()` för att kontrollera om en mapp existerar i Python, låt oss titta på hur detta fungerar bakom kulisserna.

Funktionen `os.path.exists()` använder funktionen `os.stat()` för att hämta metadata om sökvägen som har angetts. Om `os.stat()` kastar ett undantag betyder det att sökvägen inte existerar, och `os.path.exists()` returnerar `False`.

Funktionen `os.path.isdir()` använder sig också av `os.stat()`, men kollar också om sökvägen är en mapp eller inte genom att jämföra värdet på `st_mode` i resultatet från `os.stat()` mot konstanten `S_IFDIR` från modulen `stat`.

## Se även

Här är några länkar som kan vara användbara för dig när du arbetar med att kontrollera mappar i Python:

- [Dokumentation för os.path modulen](https://docs.python.org/3/library/os.path.html)
- [Dokumentation för stat modulen](https://docs.python.org/3/library/stat.html)
- [Python Tutorial: Working with directories](https://www.pythonforbeginners.com/files/working-with-directories-in-python)
- [Python File and Directory manipulation techniques](https://www.tutorialspoint.com/python/os_file_methods.htm)