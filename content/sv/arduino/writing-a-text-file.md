---
title:                "Arduino: Skriva en textfil"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Varför

Att kunna skriva en textfil är en viktig färdighet för alla Arduino-programmerare. Det gör det möjligt att spara och lagra viktig information och data för användning i dina projekt. Det kan också hjälpa till med felsökning och felrapportering.

# Hur man gör

För att skriva en textfil på Arduino behöver du först skapa en ny instans av klassen "File". Sedan kan du använda funktionen "open" för att öppna en ny fil. Till exempel:

```arduino
File minFil = SD.open("mitttextdokument.txt", FILE_WRITE);
```

Genom att ange "FILE_WRITE" som andra argumentet kommer filen att öppnas för skrivning. Om du vill öppna en befintlig fil för att läsa, behöver du bara ändra argumentet till "FILE_READ". Nu när filen är öppen kan du skriva text till den med hjälp av funktionen "println". Till exempel:

```arduino
minFil.println("Jag älskar att programmera med Arduino!");
```

För att spara filen och stänga den kan du använda funktionen "close". Det är viktigt att komma ihåg att stänga filen efter att du är klar med den.

# Djupdykning

Det finns några viktiga saker att tänka på när du skriver en textfil på Arduino. För det första är det viktigt att se till att du inte öppnar för många filer samtidigt eftersom det kan orsaka minnesproblem. Det är också viktigt att stänga filer när du är klar med dem för att frigöra minne.

En annan viktig sak att tänka på är filnamnet och var du lagrar filen. Om du använder ett SD-kort, se till att du använder det korrekta filsystemet för att kunna läsa och spara filer. Det är också bra att använda unika filnamn för varje fil du skapar för att undvika konflikter eller överlappning.

En sista viktig punkt är att förstå vilken typ av data du sparar. Om du vill spara numeriska värden måste du konvertera dem till text med hjälp av funktionen "toString". Om du inte gör det kan data sparas på ett ofullständigt sätt.

# Se också

- [Filbiblioteket i Arduino](https://www.arduino.cc/en/Reference/SD)
- [Öppna filer på SD-kort med Arduino](https://www.arduino.cc/en/Tutorial/ReadWrite)
- [Filskrivningsövning med Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite)