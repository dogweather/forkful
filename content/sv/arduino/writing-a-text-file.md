---
title:                "Skriva en textfil"
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva en textfil är en vanlig uppgift för programutvecklare. Det innebär helt enkelt att skriva en serie av text eller data till en fil som kan sparas på en dator eller en annan lagringsenhet. Programmörer använder detta för att spara information eller resultat från sina program för senare referens eller användning.

## Så här gör du:

För att skriva en textfil i Arduino, behöver du först öppna en fil med ett specifikt filnamn och välja vilket läge du vill skriva i. Sedan kan du använda funktionen ```print()``` eller ```println()``` för att skriva text eller data till filen. Se nedan för ett exempel:

```Arduino
File textfil = SD.open("min_textfil.txt", FILE_WRITE);
if (textfil) {
  textfil.println("Hej, det här är en textfil som skrivs från Arduino!");
  textfil.close();
} else {
  Serial.println("Fel vid öppning av filen!");
}
```

Detta kommer att skriva "Hej, det här är en textfil som skrivs från Arduino!" till filen "min_textfil.txt". Om filen inte finns, kommer den att skapas automatiskt.

## Djupdykning:

Att skriva en textfil är en enkel men viktig uppgift för programmerare. Det har funnits sedan de tidiga dagarna av datorer och används fortfarande flitigt idag. Alternativ till att skriva en textfil är att använda en databas eller andra filformat, men textfiler är fortfarande populära på grund av deras enkla och lättlästa format.

För att kunna skriva en textfil måste du ha en enhet som kan lagra filer, som till exempel ett SD-kort eller en USB-enhet. I Arduino finns det ett inbyggt SD-kortsfack som du kan använda för att lagra textfiler. 

## Se även:

- Mer information om SD-kortshantering i Arduino: https://www.arduino.cc/en/Reference/SD
- Tutorial om hur man skriver en textfil med Arduino: https://www.instructables.com/id/Simple-Arduino-SD-Card-Text-Reader/