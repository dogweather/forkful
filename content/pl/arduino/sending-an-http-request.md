---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP jest to żądanie, które Twoje urządzenie (w tym przypadku Arduino) przesyła do serwera, aby przetworzyć pewne informacje. Programiści robią to, aby nawiązać komunikację między urządzeniem a siecią, na przykład do pobrania danych z internetu.

## Jak to zrobić:

Aby wysłać żądanie HTTP za pomocą Arduino, używamy biblioteki WiFi. Poniżej znajduje się przykładowy kod:

```Arduino
#include <WiFi.h>

const char* ssid     = "your_SSID";
const char* password = "your_PASSWORD";

const char* host = "www.google.pl";

void setup()
{
  Serial.begin(115200);

  WiFi.begin(ssid, password);
  
  while (WiFi.status() != WL_CONNECTED) {
  delay(500);
  Serial.println("Connecting to WiFi..");
  }
  Serial.println("Connected to the WiFi network");
}

void loop()
{
  WiFiClient client;

  if (!client.connect(host, 80)) {
    Serial.println("connection failed");
    return;
  }

  client.print("GET /search?q=arduino HTTP/1.1\r\nHost: ");
  client.print(host);
  client.print("\r\nConnection: close\r\n\r\n");

  while(client.connected()){
    if(client.available()){
      String line = client.readStringUntil('\r');
      Serial.print(line);
    } else {
      break;
    }   
  }
  client.stop();
  delay(5000);  
}
```

Ten kod łączy się z siecią WiFi, wysyła żądanie HTTP do google.pl i wyświetla wyniki wyszukiwania dla "arduino".

## Deep Dive

Historia protokołu HTTP sięga 1991 roku, kiedy to został zaprojektowany do komunikacji między serwerem www a klientem. Dziś jest podstawą większości aplikacji internetowych.

Alternatywą dla HTTP jest HTTPS, zaszyfrowana wersja protokołu, która zapewnia bezpieczną komunikację.

Co do szczegółów implementacji, Arduino wykorzystuje bibliotekę WiFi, aby nawiązać połączenie z siecią, a następnie bibliotekę WiFiClient do wysyłania i odbierania żądań HTTP.

## Zobacz też

1. Introdukcja do HTTP: [link](https://developer.mozilla.org/pl/docs/Web/HTTP/Overview)
2. Dokumentacja biblioteki WiFi dla Arduino: [link](https://www.arduino.cc/en/Reference/WiFi)
3. Jak zaszyfrować swoje połączenia z HTTPS: [link](https://letsencrypt.org/pl/)