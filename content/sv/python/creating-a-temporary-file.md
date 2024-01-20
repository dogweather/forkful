---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att skapa en temporär fil innebär att generera en fil med data som endast är tillfällig i sin natur och raderas efter att den uppfyllt sitt syfte. Programmerare gör detta för att hantera stora datamängder, flytta information mellan olika delar av ett program eller skapa backuper under en igångsättande process.

## Hur gör man:

Python's `tempfile`-modulen är designad för just det här ändamålet. Nedan är ett exempel på hur man skapar en temporär fil med Python:

```Python
import tempfile

# Skapa en temporär fil
temp = tempfile.TemporaryFile()

# Skriv något i den
temp.write(b'Test text')

# Gå tillbaka till början och läs innehållet
temp.seek(0)
print(temp.read())

# Stäng och radera filen
temp.close()
```

När du kör det här skriptet, får du följande output:

```Python
b'Test text'
```

## Djupare dyk:

Att skapa temporära filer är inte unikt för Python, men det debiterades av Unix-operativsystem under 1970-talet. Alternativ inkluderar att skapa egen-hanterade temporära filer (vilket kan skapa risker för säkerhet och integritet) eller att använda minnesbaserade alternativ som Named Temporary Files.

När en temporär fil skapas i Python, görs detta med hjälp av systemanrop till operativsystemet. Python ber OS att skapa en unik filplats, och delar sedan denna plats som ett filobjekt som alla andra.

## Se även:

För mer information om att skapa temporära filer i Python, se följande länkar:

1. [Python’s Official Documentation for the tempfile Module](https://docs.python.org/3/library/tempfile.html)
2. [Python's `tempfile` — Generate Temporary Files and Directories](https://realpython.com/python-tempfile/)
3. [How To Create a Temporary File in Python](https://www.delftstack.com/howto/python/create-a-temporary-file-in-python/)