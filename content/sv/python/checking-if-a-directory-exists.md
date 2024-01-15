---
title:                "Att kontrollera om en mapp finns"
html_title:           "Python: Att kontrollera om en mapp finns"
simple_title:         "Att kontrollera om en mapp finns"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en mapp existerar kan vara en viktig del av att skapa strukturerade och effektiva program. Genom att veta om en mapp redan finns, kan du undvika fel och göra ditt program mer robust.

## Hur man gör
För att kontrollera om en mapp existerar i Python, använder du funktionen `path.exists()` från `os` biblioteket. Här är ett exempel:

```python
import os

mappnamn = "mina_mappar"

if os.path.exists(mappnamn):
    print("Mappen {} existerar redan.".format(mappnamn))
else:
    print("Mappen {} finns inte än.".format(mappnamn))
```

I detta exempel kontrollerar vi om mappen "mina_mappar" redan finns. Om mappen existerar, skrivs ett meddelande ut som bekräftar det. Om mappen inte finns, skrivs ett annat meddelande ut. Kör koden och se vad som händer!

```
Mappen mina_mappar finns inte än.
```

## Djupdykning
För att förstå hur funktionen `path.exists()` fungerar, behöver vi veta lite mer om hierarkin för filer och mappar i datorn. I en dator struktureras filer och mappar i ett hierarkiskt system, där mappar kan innehålla andra mappar och filer. Varje mapp och fil har en unik sökväg som beskriver dess plats i hierarkin. För att kontrollera om en mapp existerar, behöver vi helt enkelt jämföra den sökvägen med de sökvägar som redan finns i datorn. Detta är vad funktionen `path.exists()` gör. 

En annan viktig funktion för att kontrollera mappar är `path.isdir()`, som kontrollerar om en sökväg leder till en mapp. Om vi vill kontrollera om en sökväg leder till en fil, kan vi istället använda `path.isfile()`.

## Se även
- [Python os.path.exists() dokumentation](https://docs.python.org/3/library/os.path.html#os.path.exists)
- [Guide till filhantering i Python](https://realpython.com/working-with-files-in-python/)