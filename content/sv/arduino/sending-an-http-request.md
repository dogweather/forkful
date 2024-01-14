---
title:                "Arduino: Att skicka en http-begäran"
simple_title:         "Att skicka en http-begäran"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Att skicka en HTTP-begäran (request) är en grundläggande och viktig del av många internetbaserade projekt. Genom att göra det kan du ansluta din Arduino till olika webbtjänster och kommunicera över internet. Detta möjliggör funktioner som att hämta data, kontrollera enheter och göra uppdateringar. I den här bloggposten ska vi titta närmare på hur du kan skicka en HTTP-begäran med din Arduino.

## Hur man gör det

Att skicka en HTTP-begäran med en Arduino är inte enkelt, men med hjälp av lite kod kan vi enkelt komma igång. Först behöver vi inkludera Ethernet-biblioteket och skapa en instans av EthernetClient.

```
Arduino #include <Ethernet.h>
EthernetClient client;
```

Sedan behöver vi ansluta till internet via Ethernet-shield eller ett WiFi-shield beroende på din Arduino-modell.

```
Arduino Ethernet.begin(MAC-address); 
```

Nu kan vi börja konfigurera vår HTTP-begäran. Vi behöver ange webbadressen och önskad metod (GET, POST, osv.).

```
Arduino client.connect(server, port); 
client.print("HTTP/1.1 POST /data HTTP/1.1"); 
```

För att skicka data kan vi använda oss av `client.print()` eller `client.println()`.

```
Arduino client.println("Content-Type: application/json"); 
client.println("Content-Length: 9"); 
client.println("foo=bar"); 
```

Slutligen behöver vi avsluta med `client.println()` och stänga anslutningen när begäran är klar.

```
Arduino client.println(); 
client.println("Connection: close");
client.stop();
```

När begäran skickas och anslutningen är stängd kan vi kontrollera svaret från servern via `client.available()` och `client.read()`.

```
Arduino while (client.available()) {
  char c = client.read();
  Serial.print(c);
}
```

## Djupdykning

Att skicka en HTTP-begäran kan verka enkelt, men det finns många aspekter att ta hänsyn till. Att använda rätt metoder och skicka rätt format av data är viktigt för att få ett korrekt svar från servern. Det är också viktigt att hantera eventuella fel och felmeddelanden som kan uppstå under processen.

En annan djupare aspekt att utforska är möjligheten att integrera SSL-certifikat för säkrade anslutningar. Detta är särskilt viktigt om du skickar känslig information över internet.

## Se även

- [Arduino Ethernet Library Reference](https://www.arduino.cc/en/Reference/Ethernet)
- [Arduino WiFi Library Reference](https://www.arduino.cc/en/Reference/WiFi)
- [HTTP Requests in Arduino](https://randomnerdtutorials.com/solved-reason-http-client-failed-parse-httplength-header-arduino/)
- [Integrating SSL with Arduino](https://www.arduino.cc/sv/Guide/Bibliotek#toc5)