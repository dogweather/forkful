---
title:                "Python: **Att skriva en textfil**"
simple_title:         "**Att skriva en textfil**"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att kunna skriva textfiler är en grundläggande färdighet inom programmering och kan vara användbart i många olika situationer. Genom att kunna skapa och manipulera textfiler kan man spara data, skapa dokument och till och med bygga egna användargränssnitt.

## Hur man gör det

För att skriva en textfil i Python behöver vi först öppna en fil för skrivning. Detta kan göras genom att använda den inbyggda funktionen "open()" och ange namn och läge för filen. Sedan kan vi skriva till filen med hjälp av "write()" funktionen och stänga filen när vi är klara med "close()" funktionen. Se kodexempel nedan:

```Python
# Öppna en fil för skrivning i läge "w", vilket skapar en ny fil om den inte redan finns
fil = open("min_textfil.txt", "w")

# Skriv en sträng till filen
fil.write("Detta är en textfil som skapas med hjälp av Python!")

# Stäng filen
fil.close()
```

Om vi nu skulle öppna filen "min_textfil.txt" i en texteditor, skulle vi se innehållet som vi precis skrev. För att lägga till mer text till en befintlig fil, kan man använda läget "a" istället för "w". Se kodexempel nedan:

```Python
# Öppna en befintlig fil för skrivning i läge "a"
fil = open("min_textfil.txt", "a")

# Lägg till lite mer text till filen
fil.write(" Detta är en till sträng som läggs till!")

# Stäng filen
fil.close()
```

Med hjälp av "write()" funktionen kan vi också skriva listor, variabler och andra datastrukturer till en textfil. Det är också viktigt att notera att när man skriver till en fil, skrivs allt över det som redan finns i filen. Om man vill lägga till text på ett specifikt ställe i en fil, kan man använda "seek()" funktionen för att flytta läsmarkören till en viss position i filen.

## Djupdykning

När vi skriver till en textfil i Python måste vi se till att vi skriver rätt sorts data. Det betyder att om vi försöker skriva en variabel som innehåller en siffra eller en lista av strängar, måste vi konvertera den till en sträng innan den skrivs till filen. Detta kan göras med "str()" funktionen. Se kodexempel nedan:

```Python
# Spara en siffra i en variabel
siffra = 7

# Skriv till filen
fil.write(str(siffra))
```

Det är också viktigt att stänga filen när man är klar med den, annars kan det leda till problem som att andra program inte kan öppna filen. Om man glömmer att stänga filen, finns det en möjlighet att stänga den automatiskt med "with" konstruktionen. Se kodexempel nedan:

```Python
# Öppna filen med "with" konstruktionen
with open("min_textfil.txt", "w") as fil:

    # Skriv till filen
    fil.write("Med with konstruktionen behöver vi inte stänga filen manuellt!")
```

## Se även

För mer information om hur man skriver och hanterar textfiler i Python, kan du gärna titta på följande resurser:

- [Python Docs: Text File Handling](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Real Python: Working with Text Files in Python](https://realpython.com/read-write-files-python/)
- [GeeksforGeeks: Reading and Writing to Files in Python](https://www.geeksforgeeks.org/reading-writing-text-files-python/)