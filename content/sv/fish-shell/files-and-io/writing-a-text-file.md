---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:54.974536-07:00
description: "Hur man g\xF6r: F\xF6r att skriva till en textfil i Fish kan du anv\xE4\
  nda `echo`-kommandot i kombination med omdirigeringsoperatorer. Det finns inte popul\xE4\
  ra\u2026"
lastmod: '2024-03-13T22:44:38.356579-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att skriva till en textfil i Fish kan du anv\xE4nda `echo`-kommandot\
  \ i kombination med omdirigeringsoperatorer."
title: Att skriva en textfil
weight: 24
---

## Hur man gör:
För att skriva till en textfil i Fish kan du använda `echo`-kommandot i kombination med omdirigeringsoperatorer. Det finns inte populära tredjepartsbibliotek specifikt för att skriva till filer i Fish, eftersom skalets inbyggda kommandon är enkla och effektiva för detta ändamål.

### Skriva text till en ny fil eller skriva över en befintlig fil:
```fish
echo "Hej, Fish Shell!" > output.txt
```
Det här kommandot skriver "Hej, Fish Shell!" till `output.txt`, skapar filen om den inte finns eller skriver över den om den gör det.

### Lägga till text i en befintlig fil:
Om du vill lägga till text i slutet av en befintlig fil utan att ta bort dess nuvarande innehåll, använd append-operatören `>>`:
```fish
echo "Lägger till ny rad i filen." >> output.txt
```

### Skriva flera rader:
Du kan skriva flera rader till en fil genom att använda echo med ett nytt radtecken `\n`, eller du kan kedja ihop flera echo-kommandon med semikolon:
```fish
echo "Första raden\nAndra raden" > output.txt
# ELLER
echo "Första raden" > output.txt; echo "Andra raden" >> output.txt
```

### Exempel på utdata:
För att visa innehållet i `output.txt` efter att ha kört ovanstående kommandon, använd `cat`-kommandot:
```fish
cat output.txt
```
```plaintext
Första raden
Andra raden
```
Att ersätta eller lägga till texter som visat manipulerar filinnehållet enligt dina krav, och visar enkla men kraftfulla sätt att arbeta med textfiler i Fish Shell.
