---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:14.277339-07:00
description: "Een string kapitaliseren betekent elk karakter veranderen in een hoofdletter.\
  \ Programmeurs doen dit voor consistentie, met name in gebruikersinterfaces of\u2026"
lastmod: '2024-03-13T22:44:51.055689-06:00'
model: gpt-4-0125-preview
summary: Een string kapitaliseren betekent elk karakter veranderen in een hoofdletter.
title: Een string met hoofdletters maken
weight: 2
---

## Wat & Waarom?

Een string kapitaliseren betekent elk karakter veranderen in een hoofdletter. Programmeurs doen dit voor consistentie, met name in gebruikersinterfaces of bij het voorbereiden van gegevens voor opslag of vergelijking.

## Hoe te:

In de Arduino-omgeving is er geen ingebouwde functie om een hele string te kapitaliseren, dus we zullen een eenvoudige functie schrijven om dit te doen:

```Arduino
void setup() {
  Serial.begin(9600);
  char example[] = "hallo, wereld!";
  capitalizeString(example);
  Serial.println(example);
}

void loop() {
  // Hier hoeft niets gedaan te worden
}

void capitalizeString(char* str) {
  for (int i = 0; str[i] != '\0'; i++) {
    str[i] = toupper((unsigned char)str[i]);
  }
}
```

Na het uitvoeren van de schets toont de seriele monitor uitvoer:
```
HALLO, WERELD!
```

## Diepduiken

Historisch gezien vereist het manipuleren van strings in low-level talen zoals C het werken met individuele karakters vanwege de afwezigheid van high-level stringmanipulatiefuncties. Deze traditie zet zich voort in Arduino's C++ derivaten.

Alternatieven omvatten het gebruik van `String` objecten beschikbaar in Arduino's C++ en het aanroepen van de `.toUpperCase()` methode. Dit verbruikt echter meer geheugen. Voor geheugenbeperkte omgevingen zoals microcontrollers is het vaak beter om te werken met C-stijl karakterrijen (strings) en deze ter plaatse te manipuleren.

Implementatiedetails om te onthouden bij het kapitaliseren van een string in Arduino:
- Zorg ervoor dat de string muteerbaar is (d.w.z. een karakterrij).
- Gebruik de `toupper` functie uit `<ctype.h>` om individuele karakters te converteren.
- Stringmanipulatie kan leiden tot geheugenproblemen zoals bufferoverloop indien niet zorgvuldig behandeld.

## Zie Ook

- Arduino Referentie voor String `.toUpperCase()` methode: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/
- Cplusplus.com `toupper` referentie: http://www.cplusplus.com/reference/cctype/toupper/ 
- Arduino String manipulatie voorbeelden: https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator
