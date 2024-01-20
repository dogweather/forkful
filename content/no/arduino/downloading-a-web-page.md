---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å laste ned en webside innebærer å motta data fra en nettserver og lagre den i en lokal fil. Programmerere gjør dette for å hente og behandle data fra weben i deres prosjekter.

## Slik Gjør Du:

Her er et enkelt eksempel på hvordan du kan laste ned en webside med Ethernet-biblioteket i Arduino.

```Arduino
#include <Ethernet.h>

void setup() {
  Serial.begin(9600);

  // Oppstart Ethernet-klienten
  if (Ethernet.begin(mac) == 0) {
    Serial.println("Konfigurasjon mislyktes");
    while (true);
  }
  delay(1000);

  // Koble til serveren
  if (client.connect(server, 80)) {
    client.println("GET / HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  // Skriv ut responsen fra serveren
  if (client.available()) {
    char c = client.read();
    Serial.print(c);
  }
}
```

## Dypdykk

Laste ned websider har blitt en vanlig praksis siden webens begynnelse. Tidlig på 1990-tallet ble det vanlig å bruke protokoller som HTTP til å anmode data fra servere og lagre disse dataene lokalt.

Som et alternativ til det innebygde Ethernet-biblioteket, kan du også bruke andre nettverksbiblioteker som WiFi101, WiFiNINA eller Ethernet2 avhengig av hardwaren din.

Detaljer rundt implementeringsmetodene er varierte. Kodeeksempelet over bruker `GET`-metoden til å hente hovedsiden på webserveren ved hjelp av HTTP/1.1-protokollen. Den bruker 'client.available()' til å sjekke for tilgjengelige bytes å lese og viser deretter responsen.

## Se Mer:

- [Arduino Ethernet Library](https://www.arduino.cc/en/Reference/Ethernet)
- [Protokoller](https://www.sololearn.com/Course/Networking/))