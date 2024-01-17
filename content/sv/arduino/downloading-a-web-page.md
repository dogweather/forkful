---
title:                "Ladda ner en webbsida"
html_title:           "Arduino: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ladda ner en webbsida är när du hämtar alla data och information från en online-sida och sedan visar den på din enhet. Det är en vanlig uppgift för programmerare eftersom det låter oss få åtkomst till och använda information från hela internet i våra program. 

## Hur man:

För att ladda ner en webbsida i Arduino behöver du först använda dig av biblioteket "Ethernet". Sedan behöver du skapa en "client" som är ansluten till servern där webbsidan finns. Till exempel:

```
#include <Ethernet.h>

EthernetClient client;

IPAddress server(192,168,1,1); //ange IP-adress till servern här
const int port = 80; //ange porten servern använder

void setup() {
  //ansluta till servern
  if (client.connect(server, port)) {
    Serial.println("Anslutning nog");
    
    //skickar en begäran om att ladda ner webbsidan
    client.println("GET /index.html HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();
  } else {
    //om anslutningen misslyckas, visa felmeddelande
    Serial.println("Anslutning misslyckades");
  }
}

void loop() {
  //läser och visar data som mottagits från servern
  if (client.available()) {
    char c = client.read(); //läser en och en bokstav
    Serial.print(c); //visar i serimonitorn
  }
}
```

Om allt har gått bra så kommer webbsidan att visas i serimonitorn och du har nu lyckats ladda ner en webbsida med Arduino!

## Djupdykning:

Att ladda ner en webbsida i Arduino är en vanlig användning av Ethernet-biblioteket. Innan detta bibliotek fanns, var det betydligt svårare att få åtkomst till internet på en Arduino. Idag finns det dock också andra alternativ som gör det möjligt att ladda ner en webbsida, som till exempel Wi-Fi-shield.

För att kunna ladda ner en webbsida behöver din Arduino också kunna använda HTTP-protokollet. Detta är standardprotokollet som används för att hämta webbsidor från en server. Det finns också andra protokoll som till exempel HTTPS, som gör webbtrafiken mer säker.

En annan sak att tänka på är att vissa servrar kan kräva autentisering för att du ska få tillgång till deras innehåll. Detta kan vara särskilt användbart om du behöver hämta information från en priviligerad webbsida.

## Se även:

- Ethernet-biblioteket på Arduino:s hemsida: https://www.arduino.cc/en/Reference/Ethernet
- Wi-Fi-shield till Arduino: https://www.arduino.cc/en/Main.ArduinoWiFiShield
- Om HTTP-protokollet: https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol