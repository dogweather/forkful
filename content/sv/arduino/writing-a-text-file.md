---
title:    "Arduino: Att skriva en textfil"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil är en viktig del av att utveckla och skapa projekt med Arduino. Genom att skriva välfungerande textfiler kan du enkelt spara och läsa nödvändig information till och från dina projekt. Det kan också underlätta felsökningen och förbättra prestandan hos dina kretskort. Läs vidare för att lära dig mer om hur du kan skriva textfiler med Arduino!

## Så här gör du

När vi använder Arduino för att skriva en textfil börjar vi med att öppna en fil med namnet vi vill använda oss av. Sedan öppnar vi filen och använder kommandot "```Arduino createTXTfile(filename) ```" för att skapa en ny fil med det namnet. Sedan kan vi skriva i filen genom att använda funktionen "```Arduino write(file, data)```". Här är ett exempel på hur koden kan se ut:

"```Arduino
createTXTfile("minfil.txt");
write(file, "Hej! Det här är en textfil skapad med Arduino.");
```

Det kan också vara användbart att läsa från en textfil med Arduino. Detta kan göras med hjälp av funktionen "```Arduino read(file) ```". Om det finns något innehåll i filen kommer det att returneras som en sträng som vi kan använda i vår kod. Här är ett exempel på hur man kan läsa en textfil med Arduino:

"```Arduino
read("minfil.txt");
```

## Djupdykning

När vi skriver en textfil med Arduino kan vi också ange en parameter för att bestämma hur informationen ska skrivas till filen. Defaultvärden inställningar för filen är "WRITE" och "APPEND". "Write" betyder att filen kommer att öppnas och skrivas över med den nya informationen, medan "append" betyder att den nya informationen kommer att läggas till i slutet av filen utan att skriva över den befintliga informationen. Om detta inte specificeras kommer Arduino att använda defaultvärdet " WRITE" som standard.

Det finns också möjlighet att lägga till fler parameterar för att anpassa din textfil ytterligare, till exempel "CREATE" för att skapa en ny fil om den inte redan existerar, eller "BINARY" för att skriva binär data till filen. Det är viktigt att förstå vilken parameter som behövs för ditt specifika projekt för att skapa och manipulera textfiler på rätt sätt.

## Se även

- [Arduino Reference - createTXTfile()](https://www.arduino.cc/en/Reference/SeeAlso/createTXTfile)
- [Arduino Reference - write()](https://www.arduino.cc/en/Reference/SeeAlso/write)
- [Arduino Reference - read()](https://www.arduino.cc/en/Reference/SeeAlso/read)