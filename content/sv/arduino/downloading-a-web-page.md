---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ladda ner en webbsida innebär att hårdvara (till exempel en Arduino) begär och lagrar sidans data för senare användning. Programmörer gör detta för att analysera data, övervaka förändringar, eller återanvända innehållet på något sätt.

## Hur Man Gör:

Arduino-biblioteket som "Ethernet.h" hjälper oss att kommunicera med internet. Här är ett exempel:

```Arduino
#include <Ethernet.h>
#include <SPI.h>

byte mac[] = {0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED};
EthernetClient client;

void setup() {
  if (Ethernet.begin(mac) == 0) {
    while (true);
  }
  
  if (!client.connect("www.example.com", 80)) {
    while (true);
  }
  
  client.println("GET / HTTP/1.1");
  client.println("Host: www.example.com");
  client.println("Connection: close");
  client.println();
}

void loop() {
  if (client.available()) {
    char c = client.read();
    Serial.print(c);
  }
  
  if (!client.connected()) {
    client.stop();
    while (true);
  }
}
```

Denna kodanslutningar till www.example.com, skickar en HTTP GET-begäran, och skriver ut den mottagna data. Du kommer att se webbsidans HTML-kod i seriemonitorn.

## Djupgående:

Historiskt sett har webbsidescraping (webbsidehämtning) använts sedan webbens början. Programmeringsspråk som Perl och Python gjorde det möjligt tidigt. Arduino ger oss tillförlitliga bibliotek för detta.

Alternativ till "Ethernet.h" biblioteket inkluderar "WiFi101.h" för WiFi-anslutning och "GSM.h" för mobildata.

Tänk på att ladda ner stora mängder data kan påverka din Arduinos prestanda. Använd koncept som "streaming" för att bearbeta data i mindre bitar.

## Se Även:

- [Ethernet Library](https://www.arduino.cc/en/Reference/Ethernet): För mer information om "Ethernet.h"

- [Internet of Things (IoT) med Arduino](https://www.coursera.org/learn/arduino): En omfattande kurs att lära dig mer om hur du kopplar samman Arduino med internet.

Kom ihåg att alltid respektera webbplatsernas användarvillkor och inte överbelasta deras servrar med för många förfrågningar. God programmering!