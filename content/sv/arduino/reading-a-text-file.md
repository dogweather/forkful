---
title:                "Arduino: Läsning av en textfil"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

##Varför

Att läsa en textfil på Arduino kan vara användbart för att lagra data eller komma åt information utan behov av en internetuppkoppling.

##Så här gör du

För att läsa en textfil på Arduino behöver du först öppna filen med hjälp av funktionen `File.read()`. Sedan kan du använda `File.available()` för att kontrollera om filen är tillgänglig och `File.read()` för att läsa in en karaktär i taget. Till exempel:

```Arduino
File fil = SD.open("textfil.txt");
if (fil) {
  while (fil.available()) {
    char tecken = fil.read();
    //Gör något med tecknet här
  }
  fil.close();
}
```
Detta kodavsnitt öppnar filen "textfil.txt" och läser sedan in den en karaktär åt gången tills hela filen har lästs.

##Djupdykning

För att kunna läsa mer komplexa textfiler, som till exempel CSV-filer med kommaseparerade värden, kan det vara användbart att använda funktionen `strtok()` för att dela upp strängen i olika delar. Till exempel:

```Arduino
File fil = SD.open("data.csv");
if (fil) {
  while (fil.available()) {
    char rad[50]; //spara tillräckligt med utrymme för raden
    fil.readBytes(rad, 50); //läs in raden som en sträng
    char *datum = strtok(rad, ","); //delar upp strängen vid varje kommatecken
    char *temperatur = strtok(NULL, ","); //hämtar nästa del av strängen efter kommatecknet
    char *fuktighet = strtok(NULL, ",");
    //gör något med datan här
  }
  fil.close();
}
```

Genom att använda `strtok()` kan du läsa in specifika delar av en textfil och arbeta med dem separat.

##Se även

* [Arduino - Fil](https://www.arduino.cc/reference/en/language/functions/communication/fil/)
* [Arduino - SD-bibliotek](https://www.arduino.cc/en/Reference/SD)
* [Arduino - strtok()](https://www.arduino.cc/reference/sv/language/functions/string/bytecode/strtok/)