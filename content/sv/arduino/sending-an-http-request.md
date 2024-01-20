---
title:                "Skicka en http-förfrågan"
html_title:           "Javascript: Skicka en http-förfrågan"
simple_title:         "Skicka en http-förfrågan"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-begäran handlar om att begära data från en server. Programmerare gör detta för att kommunicera med externa system, exempelvis för att hämta väderdata, styra enheter på distans, eller upprätthålla realtidsuppdateringar.

## Så här gör du:

Kodexempel som demonstrerar hur du skickar en HTTP-begäran med Arduino:

```Arduino
#include <ESP8266WiFi.h>
  
const char* ssid = "ditt_wifi_namn";
const char* password = "ditt_wifi_lösenord";

WiFiClient client;
  
void setup() {
  Serial.begin(9600);
  WiFi.begin(ssid, password);
 
  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connecting...");
  }
}
  
void loop() {
  if (client.connect("httpbin.org", 80)) {
    client.println("GET /status/418");
    client.println("Host: httpbin.org");
    client.println("Connection: close");
    client.println();
  } else {
    Serial.println("Connection failed...");
    delay(1000);
  }
  
  while(client.available()){
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }

  client.stop();
  delay(3000);
}
```

När programmet har kopplat upp mot WiFi-nätverket, skickas en HTTP GET-begäran till `httpbin.org`. Svaret skrivs sedan ut på seriella monitorn.

## Djupdykning

Historiskt sett formally introduced in 1991, har HTTP-begäran blivit den universella metoden för att hämta data över internet. Alternativ till HTTP-begäran inkluderar WebSockets, vilka tillåter tvåvägskommunikation mellan klient och server, eller MQTT, som är en lättviktig publikations-/prenumerationbaserad protokoll som ofta används för Internet of Things-projekt.

När det gäller implementering av HTTP-begäran i Arduino, är det viktigt att notera att olika varianter av Arduino-kort kräver olika bibliotek. I exemplet ovan använde vi `ESP8266WiFi.h` biblioteket specifikt för ESP8266-baserade kort. Andra Arduino-kort kan kräva andra bibliotek, till exempel `WiFiNINA.h` för Arduino Nano 33 IoT.

## Se även

För mer information om HTTP-begäran och implementationer, se följande resurser:

- [HTTP: The Protocol Every Web Developer Must Know](https://www.tutorialspoint.com/http/index.htm)

- [Arduino ESP8266 Tutorial: Getting Started](https://randomnerdtutorials.com/esp8266-web-server-with-arduino-ide/)

- [Arduino HTTP Client Library](https://www.arduino.cc/en/Tutorial/LibraryExamples/HttpClient)