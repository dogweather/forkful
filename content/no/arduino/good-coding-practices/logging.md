---
date: 2024-01-26 00:58:42.649490-07:00
description: "Hvordan gj\xF8re det: Arduino kommer ikke med et innebygget logging-bibliotek\
  \ som noen andre milj\xF8er, men du kan implementere enkel logging til Serial-\u2026"
lastmod: '2024-03-13T22:44:41.064176-06:00'
model: gpt-4-1106-preview
summary: "Arduino kommer ikke med et innebygget logging-bibliotek som noen andre milj\xF8\
  er, men du kan implementere enkel logging til Serial-konsollen med minimalt bryderi."
title: "Loggf\xF8ring"
weight: 17
---

## Hvordan gjøre det:
Arduino kommer ikke med et innebygget logging-bibliotek som noen andre miljøer, men du kan implementere enkel logging til Serial-konsollen med minimalt bryderi. Her er et raskt eksempel for å komme i gang:

```arduino
void setup() {
  // Start seriel kommunikasjon med den gitte baud raten
  Serial.begin(9600);

  // Vent på at serielporten kobler seg til - bare nødvendig på noen kort
  while (!Serial) {
    ; // vent på at serielporten kobler seg til. Nødvendig for USB
  }

  // Logg en informasjonsmelding som indikerer at oppsettprosessen er komplett
  Serial.println("Oppsett komplett!");
}

void loop() {
  // Enkel logger som skriver ut oppetiden hvert sekund
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("Oppetid (ms): ");
    Serial.println(currentMillis);

    // Her kunne du også lagt til feilmeldinger, advarsler eller annen info.
  }
  
  // Resten av programmets logikk her...
}
```

Eksempel på Serial Utdata:
```
Oppsett komplett!
Oppetid (ms): 1000
Oppetid (ms): 2000
Oppetid (ms): 3000
...
```

## Dypdykk:
Historisk sett var ikke logging på mikrokontrollere så rett frem som på et fullverdig operativsystem. Begrensede ressurser betydde at hver byte talte, og utviklere måtte være nøye med ikke å overbelaste systemet. Med ankomsten av mer kapable kort og Arduino-plattformen som forenkler prosessen, har logging blitt mer tilgjengelig.

Mens koden over demonstrerer logging via Serial-grensesnittet, inkluderer andre metoder å skrive til et SD-kort, sende data over nettverket til en fjernserver, eller til og med vise på en liten LCD-skjerm.

Å implementere et loggesystem fører med seg hensyn som rotasjon, alvorlighetsnivå (info, debug, advarsel, feil) og ytelsesinnvirkning. På en Arduino trenger du kanskje å være oppmerksom på minnebegrensninger ved logging av komplekse datastrukturer. For fjernlogging er også sikkerheten til de overførte loggene en bekymring.

Mer avanserte løsninger som Syslog, en mye brukt loggingstandard, finnes utenfor Arduinoverdenen, men du kan integrere tredjeparts biblioteker som tilbyr tilsvarende funksjonalitet med ulike grader av kompleksitet og ressurskrav.

## Se også:
- [Arduino `Serial` referanse](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [SD-kort logging med Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [SparkFuns Data Logging skjold](https://www.sparkfun.com/products/13712)
- [TinyWeb: Et praktisk eksempel på fjernlogging med Arduino](https://www.arduino.cc/en/Tutorial/WebClientRepeating)
