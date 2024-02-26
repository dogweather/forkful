---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:19.755766-07:00
description: "Schrijven naar standaardfout (stderr) rapporteert fouten en diagnostiek\
  \ gescheiden van standaarduitvoer (stdout). Het is cruciaal voor het debuggen en\u2026"
lastmod: '2024-02-25T18:49:48.421891-07:00'
model: gpt-4-0125-preview
summary: "Schrijven naar standaardfout (stderr) rapporteert fouten en diagnostiek\
  \ gescheiden van standaarduitvoer (stdout). Het is cruciaal voor het debuggen en\u2026"
title: Schrijven naar standaardfout
---

{{< edit_this_page >}}

## Wat en Waarom?
Schrijven naar standaardfout (stderr) rapporteert fouten en diagnostiek gescheiden van standaarduitvoer (stdout). Het is cruciaal voor het debuggen en loggen, waarbij ontwikkelaars problemen kunnen isoleren zonder foutmeldingen te vermengen met reguliere programma-uitvoer.

## Hoe te:
Arduino ondersteunt van nature geen stderr, maar we kunnen dit nabootsen door naar Serial te schrijven. Stel je een LED-knipperprogramma voor met foutcontrole:

```Arduino
void setup() {
  Serial.begin(9600);
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  if(!digitalWriteCheck(LED_BUILTIN, HIGH)) {
    Serial.println("Fout: Kan LED niet hoog zetten"); // Dit is onze "stderr"
  }
  delay(1000); // Wacht een seconde
  if(!digitalWriteCheck(LED_BUILTIN, LOW)) {
    Serial.println("Fout: Kan LED niet laag zetten"); // Dit is onze "stderr"
  }
  delay(1000); // Wacht een seconde
}

bool digitalWriteCheck(int pin, int waarde) {
  // Doe alsof deze functie controleert of digitalWrite succesvol was
  digitalWrite(pin, waarde);
  // Als succes, retourneer waar, laten we voor dit voorbeeld altijd falen
  return false;
}
```

Voorbeelduitvoer:
```
Fout: Kan LED niet hoog zetten
Fout: Kan LED niet laag zetten
```

## Diepere Duik
Historisch gezien is stderr een standaardstroom in veel besturingssystemen, geïntroduceerd door Unix. In Arduino, waar een besturingssysteem ontbreekt, voeren we handmatig fouten uit met Serial.print of soortgelijk. Als je logt naar een computer, kunnen logs worden omgeleid van Serial naar een bestand, waardoor ze effectief gescheiden zijn van stdout. Gevorderde gebruikers kunnen SoftwareSerial gebruiken om stderr op verschillende hardware seriële poorten te emuleren.

## Zie Ook
- Arduino's officiële documentatie over Serial: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Unix-standaardstromen: https://en.wikipedia.org/wiki/Standard_streams
- SoftwareSerial-bibliotheek: https://www.arduino.cc/en/Reference/SoftwareSerial
