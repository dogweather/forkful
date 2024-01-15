---
title:                "Å laste ned en nettside"
html_title:           "Arduino: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne laste ned en nettside kan være nyttig for å få tilgang til informasjon eller for å automatisere en prosess som krever nettverkskommunikasjon. Det kan også være en del av et større prosjekt eller eksperiment med Arduino.

## Hvordan

Å laste ned en nettside med Arduino kan gjøres ved hjelp av en Ethernet-skjold eller en Wi-Fi-modul. Først må du koble til internett og deretter bruke et program som heter "Client" for å åpne en forbindelse til nettsiden og lese dataene. Deretter kan du lagre disse dataene og behandle dem videre etter behov.

Vi kan ta en titt på et enkelt eksempel for å laste ned en nettside og lagre den i en strengvariabel:

```Arduino
#include <Ethernet.h> 
// Initialiser Ethernet-skjoldet

byte server[] = { 192, 168, 1, 1 }; 
// Dette er IP-adressen til den nettsiden du vil laste ned

char result[256]; 
// Opprett en strengvariabel for å lagre dataene

void setup() { 
  // Sett opp Ethernet-tilkoblingen

  Ethernet.begin(mac, ip); 

  Serial.begin(9600); 
  // Konfigurer seriel port for å kunne skrive til Serial Monitor

  delay(1000); 
  // Vent i 1 sekund for at tilkoblingen skal etableres
}

void loop() {
  // Åpne forbindelsen til nettsiden og les dataene

  EthernetClient client;
  if (client.connect(server, 80)) {
    Serial.println("Koblet til nettstedet!");
    client.println("GET / HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();
  }

  // Skrive dataene til strengen variabel
  while(client.available()) {
    int len = client.read((uint8_t*)result, 255);
    result[len] = '\0';
  }

  // Skriv ut dataene til Serial Monitor
  Serial.println("Data fra nettsiden: ");
  Serial.println(result);
}
```

Nå vil du kunne se utdataene fra nettsiden i Serial Monitor vinduet.

## Dypdykk

Det finnes flere metoder for å laste ned en nettside med Arduino, og dette eksempelet bare viser en enkel måte å gjøre det på. Det er også viktig å merke seg at noen nettsider krever autentisering eller spesifikke formater for forespørsler, så du må kanskje tilpasse koden basert på dette.

Det er også verdt å nevne at du ikke bør laste ned store nettsider eller store mengder data med Arduino, da det kan påvirke ytelsen til enheten.

## Se også

- [Arduino Ethernet Bibliotek](https://www.arduino.cc/en/Reference/Ethernet)
- [Arduino Wi-Fi Bibliotek](https://www.arduino.cc/en/Reference/WiFi)
- [W3Schools HTTP Protokoll Tutorial](https://www.w3schools.com/whatis/whatis_http.asp)