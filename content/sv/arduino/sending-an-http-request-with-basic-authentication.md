---
title:                "Sända en http-förfrågan med grundläggande autentisering"
html_title:           "Arduino: Sända en http-förfrågan med grundläggande autentisering"
simple_title:         "Sända en http-förfrågan med grundläggande autentisering"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Att skicka en HTTP-förfrågan med grundläggande autentisering kan vara användbart för att säkert kommunicera med en webbtjänst eller ett API. Detta gör det möjligt för din Arduino att hämta eller skicka data till en extern källa.

## Hur man gör

För att skicka en HTTP-förfrågan med grundläggande autentisering på en Arduino, följ dessa enkla steg:

1. Förbered din kodfil genom att inkludera WifIClientSecure-biblioteket:
```Arduino
#include <WiFiClientSecure.h>
```
2. Anslut sedan till ditt trådlösa nätverk med hjälp av ditt SSID och lösenord:
```Arduino
WiFi.begin(SSID, password);
```
3. Skapa en instans av WiFiClientSecure och anslut till den server som du vill skicka en förfrågan till:
```Arduino
WiFiClientSecure client;
client.connect(server, port);
```
4. Skapa sedan en HTTP-GET-förfrågan med rätt autentiseringsuppgifter:
```Arduino
client.println("GET /endpoint HTTP/1.0");
client.println("Authorization: Basic YXNkZjpwYXNkZg=="); 
client.println();
```
5. Slutligen läs och skriv ut svaret från servern:
```Arduino
char response[256];
while (client.available()) {
  char c = client.read();
  response[strlen(response)] = c;
}
Serial.println(response);
```

## Djupdykning

Att skicka en HTTP-förfrågan med grundläggande autentisering innebär att skicka dina autentiseringsuppgifter i det format som kallas "Basic Auth". Detta innebär att användarnamn och lösenord kodas i Base64-format och bifogas som en del av förfrågan.

För att skapa Base64-kodade autentiseringsuppgifter, använd följande formel: ```base64_encode(username + ':' + password)```

Det är också viktigt att notera att vi använder protokollet HTTP/1.0 i vår förfrågan. Detta beror på att Arduino, till skillnad från en webbläsare, inte kan hantera HTTP-förfrågningar med version 1.1 som använder hållbart nätverk. För mer information om detta, läs på Arduinos dokumentation.

## Se även

- [Arduino HTTPClient dokumentation](https://www.arduino.cc/en/Reference/HTTPClientBasicAuth)
- [Base64-kodningsexempel för Arduino](https://www.arduino.cc/en/Tutorial/StringIndexOfChar)
- [HTTP/1.0 vs HTTP/1.1 förfrågningar](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)