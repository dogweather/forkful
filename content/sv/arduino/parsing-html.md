---
title:                "Tolka HTML"
date:                  2024-01-20T15:29:53.244059-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka (parsing) HTML innebär att man analyserar och extraherar data från HTML-kod. Programmerare gör detta för att interagera med webbinnehåll, exempelvis för att hämta information från en webbsida till en mikrokontroller.

## Hur gör man:

```Arduino
#include <Ethernet.h>
#include <EthernetClient.h>
#include <SPI.h>

EthernetClient client;

void setup() {
  Ethernet.begin(/* Din nätverksinfo här */);
  Serial.begin(9600);

  if (client.connect("example.com", 80)) {
    client.println("GET /index.html HTTP/1.1");
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  while (client.available()) {
    String line = client.readStringUntil('\n');
    if (line.indexOf("<title>") != -1) {
      int start = line.indexOf("<title>") + 7;
      int end = line.indexOf("</title>");
      String pageTitle = line.substring(start, end);
      Serial.println(pageTitle);
      break;
    }
  }
  
  if (!client.connected()) {
    client.stop();
  }
}

```

Sample Output:
```
Example Domain
```

## Djupdykning

Att behandla HTML-data är inte nytt. 'Web scrapers' har gjort det sedan internets barndom. Ändå har inbyggda system som Arduino ofta begränsade resurser, vilket gör HTML-parsing utmanande. Alternativ till direktparsing med Arduino är att använda en mellanhand i form av en server som kan ta hand om stora delar av datatolkningen och sedan skicka enklare data till Arduinon.

Implementationsdetaljer kan inkludera att hantera oregelbunden HTML och potentiellt stora datamängder som kan överskrida Arduinos begränsade minne. En vanlig strategi är att fokusera på specifika delar av HTML-koden och ignorera resten.

## Se även

- Arduino Ethernet-bibliotek: https://www.arduino.cc/en/Reference/Ethernet
- String klassen i Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Web scraping med Python som alternativ: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
