---
title:                "Python: Skapa en tillfällig fil"
simple_title:         "Skapa en tillfällig fil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Varför skapa en tillfällig fil i Python?

Att skapa en temporär fil i Python kan vara användbart när man behöver tillfälligt lagra data eller när man inte vill spara permanenta filer på ens dator. Det är också ett vanligt sätt att hantera och organisera data i mer komplexa program.

##Så här skapar du en tillfällig fil i Python

För att skapa en tillfällig fil i Python, behöver du importera inbyggda "tempfile" biblioteket och använda funktionen "TemporaryFile()". Sedan kan du använda den temporära filen som vanligt som du skulle använda en vanlig fil. För att se en enkel kod-exempel, se nedan:

```python
import tempfile

with tempfile.TemporaryFile() as temp_file: #Skapar en temporär fil
    #Skriva data till filen
    temp_file.write(b"Du skapade en tillfällig fil från Python!")

    #Läser data från filen
    temp_file.seek(0)
    print(temp_file.read())
```

Output:

Du skapade en tillfällig fil från Python!

I detta exempel kan vi se att vi kan skriva till den temporära filen och sedan läsa från den som vilken annan fil som helst.

##Djupdykning i skapandet av en temporär fil i Python

När du skapar en temporär fil i Python, används operativsystemets temporära filsystem för att skapa filen. Det innebär att filen kommer att sparas på en temporär plats på din dator och kommer att raderas automatiskt när den inte längre används.

En viktig sak att notera är att när du skapar en temporär fil, kommer den automatiskt att öppnas i bytes-läge. Detta innebär att om du vill skriva text till filen, måste du konvertera den till bytes först. Därför inkluderade vi "b" före texten i vårt exempel: "b" står för bytes.

Slutligen, när din kod är klar och du inte längre behöver den temporära filen, raderas den automatiskt när du stänger den eller när ditt program avslutas. Om du vill ta bort den manuellt kan du använda funktionen "temp_file.close()".

##Se även

- [Python dokumentation om tempfile](https://docs.python.org/3/library/tempfile.html)
- [Real Python artikel om att arbeta med temporära filer](https://realpython.com/working-with-files-in-python/#creating-temporary-files-and-directories)

Tack för att du läste! Hoppas denna artikel har varit till hjälp för att förstå hur man skapar en temporär fil i Python. Fortsätt lära dig och utforska allt som detta kraftfulla språk har att erbjuda. Lycka till med dina programmeringsprojekt!