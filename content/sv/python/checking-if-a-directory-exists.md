---
title:    "Python: Kontrollera om en mapp finns"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en mapp finns kan vara en viktig del av att hantera filer och program på din dator. Genom att ha en enkel och effektiv process för att kontrollera om en mapp finns, kan du undvika fel och tidsödande problem när du arbetar med dina program.

## Så här gör du
För att kontrollera om en mapp finns i Python, använder vi funktionen "os.path.exists()". Här är ett enkelt exempel på hur man använder den:

```Python
import os

# Ange sökväg till mappen
folder = "C:/Users/Användare/MinMapp"

# Kontrollera om mappen finns
if os.path.exists(folder):
    print("Mappen finns!")
else:
    print("Mappen finns inte!")
```

Om mappen "MinMapp" finns kommer utskriften att vara "Mappen finns!", annars kommer den att vara "Mappen finns inte!". Det är enkelt och effektivt att använda denna funktion för att kontrollera om en mapp finns i Python.

## Djupdykning
För att förstå hur funktionen "os.path.exists()" fungerar, låt oss ta en titt på lite djupare förklaring. I grunden kontrollerar den om den angivna sökvägen finns i ditt filsystem och om den är giltig. Om sökvägen leder till en mapp, returneras True, annars returneras False.

Det är också värt att notera att denna funktion inte bara fungerar för mappar, utan också för filer. Så om du behöver kontrollera om en specifik fil finns, kan du också använda "os.path.exists()" för det.

## Se även
- [Dokumentation för os.path.exists()](https://docs.python.org/3/library/os.path.html#os.path.exists)
- [Kontrollera om en fil finns i Python](https://www.w3schools.com/python/python_file_exists.asp)