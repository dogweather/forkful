---
title:    "Python: Läsa en textfil"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en grundläggande uppgift inom programmering och kan vara användbart för att hantera stora mängder data eller för att manipulera och analysera information. Att förstå hur man läser en textfil är en viktig grundläggande kunskap för att kunna arbeta med data i Python.

## Så här gör du

För att läsa en textfil i Python behöver vi först öppna filen med hjälp av inbyggda funktionen `open()` och ange sökvägen till filen. Sedan kan vi använda en `for` loop för att iterera över varje rad i filen och skriva ut den.

```python
file = open("textfil.txt") #Öppnar filen
for line in file: #Loopar över varje rad i filen
    print(line) #Skriv ut raden
file.close() #Stäng filen när vi är färdiga
```

Om vi har en fil med namnet `textfil.txt` som innehåller följande rader:

```
Hej!
Det här är en textfil.
Hoppas du har en bra dag!
```

Så kommer koden ovan att ge följande utmatning:

```
Hej!
Det här är en textfil.
Hoppas du har en bra dag!
```

Genom att använda `rstrip()` funktionen kan vi även ta bort eventuella extra radbrytningar från varje rad i filen.

```python
file = open("textfil.txt")
for line in file:
    print(line.rstrip()) #Ta bort radbrytning från varje rad
file.close()
```

## Djupdykning

En textfil är en enkel form av fil som innehåller text och kan läsas av både människor och datorer. I Python behöver vi inte ange någon specifik filtyp när vi läser en fil, så länge innehållet är i textformat.

En fil skapas genom att man anger sökvägen till filen, namnet på filen och vilket format man vill använda. Det finns även möjlighet att skapa en fil med specifika inställningar, till exempel om man vill att filen ska öppnas i endast läsläge eller även i skrivläge.

Det finns många olika sätt att läsa en textfil i Python, beroende på vad man vill uppnå. Det kan vara att läsa in data för vidare behandling, jämföra data från flera filer eller helt enkelt bara visa innehållet på skärmen. Genom att använda funktioner som `split()` och `join()` kan man även separera och sammanfoga text från filen för att skapa en mer strukturerad utmatning.

## Se även

- [Dokumentation om filhantering i Python](https://docs.python.org/sv/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Träna på att läsa textfiler med Python](https://www.w3resource.com/python-exercises/file/python-io-exercise-6.php)
- [Läsning av textfiler i Python: En användbar tutorial](https://stackabuse.com/reading-a-text-file-in-python/)