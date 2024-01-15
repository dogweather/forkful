---
title:                "Nedladdning av en webbsida"
html_title:           "Arduino: Nedladdning av en webbsida"
simple_title:         "Nedladdning av en webbsida"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Funderar du på att ladda ner en webbsida med din Arduino? Det kan vara bra för att spara information från en specifik sida för framtida användning eller för att skapa ett projekt som hämtar information från internet.

## Hur man gör

För att ladda ner en webbsida med Arduino behöver du först ansluta din Arduino till internet via en Ethernet-kabel eller en WiFi-modul. Du behöver även kunna hantera HTML och TCP/IP protokollet. Nedan följer ett exempel på kod som hämtar en specifik URL och skriver ut innehållet på en LCD-skärm:

```
Arduino kodexempel:

#include <SPI.h>
#include <Ethernet.h>
#include <LiquidCrystal.h>

//Ange webbadressen för den sida du vill hämta
char server[] = "www.minhemsida.se";

//Skapa en Ethernet-klient som kan ansluta till internet
EthernetClient client;

//Skapa en LCD-skärm med 2 rader och 16 tecken
LiquidCrystal lcd(2, 3, 4, 5, 6, 7);
 
void setup() {
  //Sätt upp LCD-skärmen
  lcd.begin(16, 2);
  lcd.write("Laddar sida...");
  
  //Anslut till webbadressen
  if (client.connect(server, 80)) {
    lcd.clear();
    lcd.write("Ansluten!");
    
    //Skicka en GET request till servern
    client.println("GET / HTTP/1.1");
    client.println("Host: " + String(server));
    client.println("Connection: close");
    client.println();
  } else {
    lcd.clear();
    lcd.write("Kunde inte ansluta");
  }
}
 
void loop() {
  //Läs och skriv ut svaret från servern tills det är klart
  while(client.available()) {
    char c = client.read();
    lcd.write(c);
  }
}
```

Detta är bara ett enkelt exempel på hur du kan ladda ner och skriva ut en webbsida med Arduino. Det finns många andra möjligheter och det är bara din fantasi som sätter gränserna!

## Djupdykning

För att ladda ner en webbsida med Arduino behöver du ha förståelse för HTML och TCP/IP-protokollet. Du behöver också veta vad en GET-request är och hur man skickar en sådan med sin Arduino. Det finns många olika bibliotek och exempel som kan hjälpa dig att komma igång. Se till att kolla in dokumentationen för dessa för att få en bättre förståelse för hur det fungerar.

## Se även

Här är några användbara länkar för att hjälpa dig att komma igång med att ladda ner en webbsida med Arduino:

- [Arduino Ethernet-biblioteket](https://www.arduino.cc/en/Reference/Ethernet)
- [GET Request](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/GET)
- [Exempel på kod för att ladda ner en webbsida](https://www.arduino.cc/en/Tutorial/WebClient)