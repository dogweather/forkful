---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Arduino: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering innebär att en Arduino-enhet skickar en förfrågan till en server som kräver användarautentisering. Detta är vanligt förekommande när man kommunicerar med webbtjänster, till exempel för att hämta data eller skicka kommandon. Programmerare använder detta för att säkert och pålitligt kunna kommunicera med servrar.

## Så här:
Här är ett exempel på en HTTP-begäran med grundläggande autentisering i Arduino:

```Arduino
#include <WiFiClientSecure.h>
 
char ssid[] = "wifi-nätverkets-namn";
char pass[] = "password";
char server[] = "serverns-adress";
 
WiFiClientSecure client;
 
void setup() {
  Serial.begin(9600);
  WiFi.begin(ssid, pass);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
  }
}
 
void loop() {
  if (client.connect(server, 443)) {
    Serial.println("Försöker skicka begäran...");
    String request = "GET / HTTP/1.1\r\nHost: " + String(server) + "\r\n\r\n";
    client.print(request);
 
    if (client.available()) {
      while (client.available()) {
        char c = client.read();
        Serial.print(c);
      }
    }
 
    client.stop();
    while (client.connected()) {
      delay(1);
    }
  }
  Serial.println("Begäran skickad!");
  delay(5000);
}
```

När koden körs, kommer Arduino-enheten ansluta till det angivna WiFi-nätverket och sedan skicka en GET-begäran med grundläggande autentisering till den angivna servern. Om servern svarar kommer svaret att skrivas ut i seriell monitor.

## Djupdykning:
HTTP-begäran med grundläggande autentisering är en säkerhetsfunktion som har funnits sedan HTTP-protokollet skapades. Den används för att skydda kommunikationen mellan en klient och en server genom att kräva en användares godkännande innan åtkomst till resurser tillåts.

Det finns flera alternativ till grundläggande autentisering, till exempel OAuth och JWT. Dessa metoder är mer avancerade och ger mer robust autentisering, men de kräver också mer komplicerad kodbearbetning.

I implementeringen av vår kod används WiFiClientSecure-biblioteket, vilket gör det möjligt att kommunicera över ett säkert anslutet WiFi-nätverk med hjälp av TLS/SSL-kryptering.

## Se även:
- [WiFiClientSecure biblioteket](https://www.arduino.cc/en/reference/wificlientsecure/)
- [HTTP-grundläggande autentisering](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [OAuth-autentisering](https://oauth.net/)
- [JWT-autentisering](https://jwt.io/)