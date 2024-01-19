---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Parsing av HTML er prosessen når en HTML-dokument blir analysert og konvertert til en struktur som er enkel for programmerere å forstå og jobbe med. Vi gjør dette for å lette datautvinning og web skraping.

## Hvordan gjør man det:

Her er enkel kode for å parse HTML ved hjelp av en Arduino Ethernet skjold og Ethersheild bibliotek. Vi bruker Google-hjemmesiden som eksempel:

```Arduino
#include <EtherShield.h>
 
// Definer nettverksdetaljer
byte mac[] = {0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED};
byte ip[] = {10, 0, 0, 2};
byte gw[] = {10, 0, 0, 1};
byte dns[] = {8, 8, 8, 8};

// Opprett en Ethernet-klient
EthernetClient client;

void setup() {
  Ethernet.begin(mac, ip, dns, gw);
  Serial.begin(9600);

  // Koble til Google
  if (client.connect("www.google.com", 80)) {
    Serial.println("Connected to Google");
    // Send en HTTP-forespørsel
    client.println("GET / HTTP/1.0");
    client.println();
  }
  
  // vent og motta data
  while (client.available()) {
    char c = client.read();
    Serial.print(c);
  }

  // Lukk tilkoblingen
  client.stop();
}

void loop() {
  // Ikke noe som trengs her
}
```
Eksempelutgang vil være rå HTML-kode fra Google-hjemmesiden.

## Dyp Dykk

Parsing av HTML har vært standard siden opprettelsen av web-skraping, hvor programmerere trengte å trekke ut spesifikk data fra nettsider. Ytterligere alternativer for parsing inneholder biblioteker som Beautiful Soup (Python), Jsoup (Java) og HtmlAgilityPack (.NET).

HTML Parser bruker generelt en teknikk kjent som Tre-gående, hvor Parser går gjennom HTML-treet og utfører spesifikke handlinger basert på nodetype.

## Se Også

1. Arduino Ethernet Shield dokumentasjon: https://www.arduino.cc/en/Main/ArduinoEthernetShield
2. Ethershield bibliotek: https://www.arduino.cc/en/Reference/Ethernet
3. "Tregående algoritmer" for mer dybde på hvordan parsing teknikker jobber: https://en.wikipedia.org/wiki/Tree_traversal