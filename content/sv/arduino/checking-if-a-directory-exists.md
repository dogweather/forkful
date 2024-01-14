---
title:                "Arduino: Kontrollera om en mapp finns"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en mapp finns är en viktig del av programmering, speciellt när du arbetar med hårdvara som en Arduino. Genom att kontrollera om en mapp existerar kan du effektivt hantera och organisera dina data och filer.

## Hur man gör
Att kontrollera om en mapp finns i Arduino kräver användning av ett bibliotek som heter `SD`. Detta bibliotek tillåter dig att hantera SD-kortet som är anslutet till din Arduino och ger dig tillgång till filerna och mapparna på det.

Här är ett enkelt exempel på hur du kan använda `SD.exists()`-funktionen för att kontrollera om mappen "Data" finns på ditt SD-kort:

```Arduino
#include <SD.h>

void setup() {
  if (SD.exists("Data")) {
    Serial.println("Mappen Data finns.");
  } else {
    Serial.println("Mappen Data finns inte.");
  }
}

void loop () {

}
```

Om mappen "Data" existerar så kommer texten "Mappen Data finns." att skrivas ut i seriell monitor, annars kommer "Mappen Data finns inte." att skrivas ut.

Du kan också använda `SD.isDirectory()`-funktionen för att kontrollera om en fil är en mapp eller inte. Här är ett exempel:

```Arduino
#include <SD.h>

void setup() {
  if (SD.isDirectory("Data")) {
    Serial.println("Filen Data är en mapp.");
  } else {
    Serial.println("Filen Data är inte en mapp.");
  }
}

void loop () {

}
```

I detta exempel kommer "Filen Data är en mapp." att skrivas ut eftersom vi använder `SD.isDirectory()`-funktionen för att kontrollera en mapp.

## Djupdyka
När du använder `SD.exists()`-funktionen för att kontrollera en mapp, så kommer funktionen att söka igenom både filer och mappar. Detta innebär att om du till exempel har en fil som heter "Data" på ditt SD-kort, så kommer `SD.exists("Data")` att returnera `true` även om det inte är en mapp.

Det är också viktigt att komma ihåg att SD-kortets filsystem är FAT16 eller FAT32, vilket betyder att mappnamn är begränsade till åtta tecken plus en trebokstavlig filändelse. Detta kan leda till problem om du har flera mappar med samma första åtta tecken.

## Se också
- [SD Library](https://www.arduino.cc/en/reference/SD)
- [TutorialsPoint - Arduino SD Library](https://www.tutorialspoint.com/arduino/arduino_sd_card.htm)
- [How to Use an SD Card with Arduino](https://create.arduino.cc/projecthub/Arduino_Genuino/how-to-use-an-sd-card-with-an-arduino-e0a901)