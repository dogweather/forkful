---
title:                "Arduino: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Att kunna ladda ner en webbsida kan vara användbart om man vill automatisera datainsamlingen eller analysera informationen på en viss sida. Det kan också vara en bra grund för att bygga en webbskrapa, som kan samla in data från flera olika webbsidor.

## Hur man gör

För att ladda ner en webbsida med Arduino behöver man först och främst lära sig hur man kommunicerar med internet via ett Ethernet Shield. Sedan kan man använda sig av ett open source-bibliotek som heter WebClient för att hämta webbsidor. Nedan följer ett exempel på kod för att ladda ner en webbsida:

```
Arduino ... 

#include <SPI.h>
#include <Ethernet.h>
#include <WebClient.h>

byte mac[] = {0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED}; // MAC-adress för Ethernet Shield
IPAddress ip(192, 168, 1, 177); // IP-adress för Ethernet Shield
char server[] = "www.example.com"; // Adress till den webbsida som ska hämtas

EthernetClient client; // Skapa en Ethernet Client för att ansluta till webbservern
WebClient web(client); // Skapa en WebClient med Ethernet Clienten som parameter

// Anslut till internet med Ethernet Shield
if (Ethernet.begin(mac) == 0) {
    Serial.println("Failed to configure Ethernet using DHCP");
}

// Skicka en GET-request till servern och hämta webbsidan
web.get(server);

// Skriv ut webbsidans innehåll
while (client.connected() && !client.available());
while (client.available()) {
    char c = client.read();
    Serial.print(c);
}
```

Detta kodexempel visar hur man kan ansluta till en webbserver och hämta en specifik webbsidas innehåll med hjälp av WebClient-biblioteket. Det är viktigt att koden anpassas efter den specifika webbsidan som ska hämtas, såsom att ändra IP-adress och serveradress.

## Deep Dive

När man programmerar med Arduino kan det vara värt att notera att det finns olika sätt att ansluta till internet, såsom Ethernet Shields, WiFi Shields eller ESP8266 WiFi-moduler. Var och en av dessa har sin egen kodstruktur och det kan vara bra att kolla vilka bibliotek som är tillgängliga för den specifika hårdvaran.

När det gäller att hämta webbsidor är det också viktigt att känna till att det finns vissa begränsningar för hur stora sidorna kan vara och hur mycket information som kan hämtas på en gång. Om man behöver hämta stora mängder data can det vara bättre att dela upp hämtningen i flera mindre delar.

## Se även

Här nedan följer några användbara länkar för att lära sig mer om hur man kan hämta webbsidor med Arduino:

- [Ethernet Shield](https://www.arduino.cc/en/Reference/Ethernet)
- [WiFi Shield](https://www.arduino.cc/en/Reference/WiFi)
- [ESP8266 WiFi-modul](https://www.arduino.cc/en/Reference/libraries)
- [WebClient biblioteket](https://github.com/yvesdelhaye/Arduino_WebClient)
- [OpenWeatherMap API - hämta väderdata med Arduino](https://create.arduino.cc/projecthub/microcontrollerslab/getting-weather-dataA7b3Gt)