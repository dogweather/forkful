---
title:                "Arduino: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Arduino er et populært programmeringsverktøy blant hobbyister og tinkerers. En av grunnene til dette er dens evne til å kommunisere med andre enheter, som nettverkstilkoblinger. Å sende en HTTP-forespørsel med Arduino kan tillate deg å hente informasjon fra nettsteder, eller til og med kontrollere ulike enheter eksternt.

## Hvordan gjøre det

For å kunne sende en HTTP-forespørsel med Arduino, må du først sette opp et WiFi- eller Ethernet-kort. Deretter kan du bruke Arduino HTTP-klientbibliotek for å sende forespørselen. Her er et enkelt eksempel på hvordan dette kan gjøres:

```Arduino
#include <SPI.h>
#include <Ethernet.h>

// Opprette et Ethernet-objekt
EthernetClient client;

// Angi nettadressen til nettstedet du vil sende en forespørsel til
char server[] = "www.example.com";

// Funksjonen som sender forespørselen
void sendRequest() {
  if (client.connect(server, 80)) { // Koble til nettstedet på port 80
    client.println("GET / HTTP/1.1"); // Angi forespørselsmetode og ressurs
    client.println("Host: www.example.com"); // Angi nettadressen som header
    client.println("Connection: close"); // Informer om at du vil avslutte tilkoblingen etter respons
    client.println(); // Avslutt header
  }
}

void setup() {
  // Start seriell kommunikasjon for feilsøking
  Serial.begin(9600);

  // Initialisere Ethernet-objektet med MAC-adresse
  byte mac[] = {0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED};
  Ethernet.begin(mac);

  // Vent på at Ethernet-tilkobling er oppnådd
  while (Ethernet.linkStatus() == LinkOFF) {
    delay(500);
  }
}

void loop() {
  sendRequest(); // Kall funksjonen som sender forespørselen

  // Les svaret fra serveren og skriv ut til seriell monitor
  while (client.available()) {
    char c = client.read();
    Serial.print(c);
  }

  // Vent 5 sekunder før du sender en ny forespørsel
  delay(5000);
}
```

Eksempelet viser hvordan du kan sende en GET-forespørsel til nettstedet www.example.com og skrive ut svaret til seriell monitor. Du kan endre nettadressen og forespørselsmetoder til dine egne behov.

## Dypdykk

Når du sender en HTTP-forespørsel med Arduino, er det viktig å forstå hvordan nettverkstilkoblingen fungerer for å unngå problemer som ikke-responderte forespørsler eller uventet avslutning av tilkoblingen. Det er også viktig å sørge for at du skriver riktig forespørselsformat og inkluderer nødvendige headere.

Du kan også utforske forskjellige måter å behandle responsen på, for eksempel å bruke JSON-parsing til å hente spesifikke data fra responsen.

## Se også

- [Arduino HTTP-klientbibliotek](https://www.arduino.cc/en/Reference/HttpClient)
- [HTTP-forespørsler og responskoder](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)
- [Kommunikasjon med nettverk i Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/EthernetWebClient)