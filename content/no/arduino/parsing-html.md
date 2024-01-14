---
title:                "Arduino: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/parsing-html.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvis du er en Arduino-entusiast og ønsker å lære mer om hvordan du kan hente og behandle data fra nettet, kan HTML-parsing være en nyttig ferdighet å lære. Ved å kunne analysere HTML-koden til en nettside, kan du hente ut spesifikke data og bruke dem til å styre dine Arduino-prosjekter. Dette kan være nyttig for alt fra å vise værdata på en skjerm til å automatisere styringen av lys og andre enheter.

# Hvordan gjøre det

For å analysere HTML-koden til en nettside med Arduino, trenger du først å kjenne til nettsiden du ønsker å hente data fra. Deretter må du bruke en Ethernet-skjold eller en WiFi-modul for å koble Arduino til internett. Når dette er gjort, kan du bruke en HTTP-klient for å sende en forespørsel til nettsiden og få tilbake HTML-koden. Etter dette kan du bruke en parser, for eksempel RoboParser, for å ekskludere unødvendig informasjon og hente ut de dataene du trenger.

La oss ta et enkelt eksempel på å hente værdata fra nettsiden til Yr. Først må vi definere noen variabler:

```Arduino
#include <SPI.h> 
#include <Ethernet.h> 

byte mac[] = {0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED}; // MAC-adresse til Ethernet-skjoldet 
EthernetClient client; // Oppretter en EthernetClient 

char server[] = "www.yr.no"; // Adressen til nettsiden 
int serverPort = 80; // Porten for HTTP-forespørsler 

String html; // String for å lagre HTML-koden vi får fra nettsiden 
```

Nå kan vi koble oss til nettsiden og hente HTML-koden ved å sende en HTTP-forespørsel:

```Arduino
if (client.connect(server, serverPort)) {
    client.println("GET /sted/Norge/Nordland/Bodø/Bodø/varsel.xml HTTP/1.1"); // Forespørsel for å få værdata for Bodø 
    client.println("Host: www.yr.no"); 
    client.println("Connection: close"); 
    client.println(); 
}
```

Deretter kan vi bruke en parser til å skille ut dataene vi er interessert i, for eksempel temperatur:

```Arduino
while (client.available()) { 
    char c = client.read(); // Leser inn en og en bokstav fra responsen 
      
    if (c == '

') { // Om vi når slutten på en linje 
        html = ""; // Nullstiller stringen 
    } else { 
        html += c; // Legger til bokstaven i Stringen 
   
        if (html.endsWith("temperature value")) { // Om Stringen slutter med "temperature value" 
            int tempIndex = html.indexOf('=', 26); // Finner indeksen til temperaturen 
            String temp = html.substring(tempIndex+1, html.indexOf('"', tempIndex+1)); // Henter ut temperaturen 
            Serial.println("Temperature: " + temp + "C"); // Skriver temperaturen til seriell monitor 
        } 
    } 
} 
```

# Dykk ned i detaljene

HTML-parsing er en krevende oppgave og det kan være lett å gjøre feil. Det er derfor viktig å være nøye med å forstå strukturen i HTML-koden du ønsker å analysere, og å bruke en parser som passer til dine behov. Det viktigste er å forstå hvordan dataene er strukturert og hvordan du kan hente dem ut ved å bruke riktige kommandoer og funksjoner.

See Also:
- http://www.htmlgoodies.com/primers/html/article.php/3478171
- https://www.arduino.cc/reference/en/language/functions/communication/client/
- https://playground.arduino.cc/Code/HTMLParseLibrary/