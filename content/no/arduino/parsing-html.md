---
title:                "«Parsing av html»"
html_title:           "Arduino: «Parsing av html»"
simple_title:         "«Parsing av html»"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Å parse HTML kan være nyttig hvis du vil hente ut spesifikk informasjon fra en nettside, for eksempel for å lage en vær-app som viser temperaturen fra en nettside. Det kan også være nyttig for å lage automatiserte prosesser, som å hente ut data fra et nettsted og lagre det i en database.

## Slik gjør du det

Du trenger en Arduino, en LCD-skjerm og et Ethernet Shield. Følg disse stegene for å parse HTML med Arduino:

1. Åpne Arduino-programvaren og opprett et nytt sketch.
2. Koble Ethernet Shield til Arduino og koble en Ethernet-kabel fra shieldet til internett.
3. Kopier følgende kode inn i sketchet:

```Arduino
#include <SPI.h>
#include <Ethernet.h>
 
byte mac[] = {0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED}; //MAC-adresse til Ethernet Shield
char server[] = "www.nettside.no"; //nettsiden du vil parse
byte serverIp[] = {x,x,x,x}; //IP-adresse til nettsiden
 
EthernetClient client;
 
void setup() {
  Serial.begin(9600); //initialiser serielkobling
  Ethernet.begin(mac); //koble til internett
  Serial.println("Connecting...");
  if (client.connect(serverIp, 80)) { //koble til nettsiden
    Serial.println("Connected");
    client.println("GET / HTTP/1.1"); //hent nettsiden
    client.println("Host: www.nettside.no");
    client.println("Connection: close");
    client.println();
  }
}
 
void loop() {
  if (client.available()) { //hvis det er data tilgjengelig
    char c = client.read(); //les dataen og skriv den til serielkoblingen
    Serial.write(c);
  }
 
  if (!client.connected()) { //hvis tilkoblingen er brutt
    Serial.println();
    Serial.println("Disconnecting.");
    client.stop(); //stopp tilkoblingen
    while(true); //stopp programmet
  }
}
```

4. Bytt ut "www.nettside.no" og "x,x,x,x" med riktig nettside- og IP-adresse.
5. Last opp koden til Arduinoen.
6. Åpne serielkoblingen i Arduino-programmet for å se resultatet.
7. Ser du HTML-koden fra nettsiden? Da har du klart å parse HTML med Arduino!

## Dypdykk

Når du bruker Ethernet Shield med Arduino for å hente en nettside, blir dataen returnert som et HTTP-svar. Dette svaret inkluderer både headers og kode, inkludert HTML-koden. For å få ut kun HTML-koden, kan du bruke en klasse som `String`. For eksempel:

```Arduino
String response = "";
while (client.available()) { //les dataen fra serveren
  char c = client.read();
  response += c;
}
int startIndex = response.indexOf("<html>"); //få indeksen til starten av HTML-koden
int endIndex = response.indexOf("</html>"); //få indeksen til slutten av HTML-koden
String htmlCode = response.substring(startIndex, endIndex); //bruk substring for å få ut HTML-koden
```

Det er også mulig å bruke biblioteker som "ArduinoHTTPClient" for å gjøre dette på en enklere måte.

## Se også

- [ArduinoHTTPClient bibliotek](https://github.com/arduino-libraries/ArduinoHttpClient)
- [Ethernet bibliotek](https://www.arduino.cc/en/Reference/Ethernet)
- [HTTP protokoll](https://developer.mozilla.org/en-US/docs/Web/HTTP)