---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering innebär att lägga till autentiseringsinformation i begäran för att säkerställa att den kan känna igen avsändaren. Programmerare gör detta för att tillhandahålla en säkerhetsnivå mellan klient och server.

## Hur man gör:
Här är koden för att skicka HTTP-begäran med användarnamn och lösenord för autentisering i Arduino.

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "your_SSID";
const char* password = "your_PASSWORD";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connecting to WiFi..");
  }
}

void loop() {
  if (WiFi.status() == WL_CONNECTED) {
    HTTPClient http;

    http.begin("http://your_server.com"); //Specify destination
    http.addHeader("Content-Type", "text/plain");  
    http.setAuthorization("username", "password"); //Specify username and password

    int httpResponseCode = http.GET();  
                                             
    if (httpResponseCode>0) {
      String response = http.getString();   
      Serial.println(httpResponseCode);
      Serial.println(response);
    }  
    else {
      Serial.print("Error on sending request: ");
      Serial.println(httpResponseCode);
    }

    http.end(); 
  }

  delay(30000);  
}
```

## Djup dykning
Grundläggande autentisering har varit en funktionalitet sedan HTTP/1.0, och det är en enkel metod för autentisering mellan klient och server. 

En alternativ metod är att använda token-baserad autentisering, till exempel JWT (JSON Web Tokens). Denna metod används ofta för att ge säker åtkomst till RESTful APIs. 

Beträffande implementeringsdetaljer, konstruerar vi HTTP-begäran, inkluderar autentiseringsinformationen (användarnamn och lösenord) i headers, och skicka begäran till den angivna servern.

## Se även
2. [RFC 7617](https://tools.ietf.org/html/rfc7617) för mer information om grundläggande autentisering.
3. En bra artikel på svenska om [Token-baserad autentisering](https://www.minhembio.com/forum/index.php?showtopic=337993).