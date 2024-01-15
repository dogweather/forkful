---
title:                "Skicka en http-begäran"
html_title:           "Arduino: Skicka en http-begäran"
simple_title:         "Skicka en http-begäran"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför
Det finns flera skäl till varför du skulle vilja skicka en HTTP-begäran med Arduino. Det kan till exempel vara för att hämta data från en webbplats eller för att kontrollera en fjärranordning över internet.

## Hur man gör det
För att skicka en HTTP-begäran med Arduino behöver du använda dig av två bibliotek: WiFi- och HTTP-klienten. Här är ett enkelt exempel på hur du kan göra det:

```arduino
#include <WiFi.h>
#include <HTTPClient.h>

// Användarnamn och lösenord för WiFi-nätverket
const char* ssid = "Ditt_Nätverk";
const char* password = "Ditt_Lösenord";

// URL till webbplatsen du vill hämta data från
String url = "https://exempelsida.com/api/data";

void setup() {
  // Anslut till WiFi-nätverket
  WiFi.begin(ssid, password);
  
  // Vänta tills anslutningen är etablerad
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
  }
  
  // Skapa en HTTP-klient
  HTTPClient http;
  // Skicka en GET-begäran till den angivna URL:en
  http.begin(url);
  // Skriv ut statuskoden från servern
  Serial.println(http.GET());
  // Skriv ut svaret från servern
  Serial.println(http.getString());
  // Stäng anslutningen
  http.end();
}

void loop() {
  // Ingenting behöver göras i loop-funktionen eftersom vi bara behöver skicka en begäran en gång
}
```

När koden körs kommer den att ansluta till det angivna WiFi-nätverket och skicka en GET-begäran till den angivna URL:en. Sedan skrivs statuskoden från servern och svaret ut i seriell överföring.

## Djupdykning
När du skickar en HTTP-begäran med Arduino använder du HTTP-protokollet för att kommunicera med en webbserver. Det finns olika typer av begäranden som kan skickas, till exempel GET, POST, PUT och DELETE. Dessutom kan du skicka med data som en del av begäran, till exempel genom att lägga till parametrar eller skicka en JSON-sträng. Det är viktigt att undersöka dokumentationen för den webbplats eller den API som du ska kommunicera med för att se vilken typ av begäran och data som krävs.

## Se även
- [Arduino WiFi-bibliotek](https://www.arduino.cc/en/Reference/WiFi)
- [Arduino HTTP-klient bibliotek](https://www.arduino.cc/en/Reference/HTTPClient)
- [HTTP-begäran på Arduino forumet](https://forum.arduino.cc/index.php?topic=249229.0)