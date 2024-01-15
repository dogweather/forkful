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

## Dlaczego

Wysyłanie żądania HTTP jest niezbędnym elementem programowania Arduino. Jest to sposób na komunikację z Internetem, co umożliwia urządzeniu wykonywanie różnych zadań, takich jak pobieranie danych, aktualizacja oprogramowania i wiele więcej.

## Jak to zrobić

Aby wysłać żądanie HTTP za pomocą Arduino, potrzebujemy biblioteki "WiFiClient.h". W celu ułatwienia, pobierzmy ją z menedżera bibliotek w Arduino IDE. Następnie skorzystajmy z poniższego kodu:

```arduino
#include <WiFi.h>
 
const char* ssid = "nazwa_sieci";
const char* password = "hasło_sieci";
 
void setup() {
  Serial.begin(115200);
  delay(1000);
  Serial.println("Łączenie z siecią WiFi...");
 
  WiFi.begin(ssid, password);
 
  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Nie można połączyć się z siecią WiFi!");
  }
 
  Serial.println("Połączono z siecią WiFi!");
}
 
void loop() {
  WiFiClient client;
 
  if (client.connect("www.example.com", 80)) {
    Serial.println("Połączono z serwerem!");
    client.println("GET / HTTP/1.0");
    client.println();
  }
 
  while (client.available()) {
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }
 
  client.stop();
  delay(5000);
}
```

Kod ten pozwala Arduino na połączenie się z naszą siecią WiFi oraz z serwerem, na którym będzie wysyłane żądanie. Następnie za pomocą funkcji "print" wysyłamy żądanie GET na stronie "www.example.com". W odpowiedzi na to otrzymujemy kod źródłowy strony, który możemy przetworzyć lub wyświetlić na serial monitorze.

## Głębszy zanurzenie

Wysyłanie żądań HTTP za pomocą Arduino może wymagać dodatkowych informacji. Na przykład, jeśli chcemy wysłać żądanie do strony wymagającej uwierzytelnienia, będziemy musieli dodać nagłówek z naszymi danymi logowania.

```arduino
client.println("Authorization: Basic dXNlcjpwYXNzd29yZA=="); // Dane logowania (login:password) w formacie Base64
```

Możemy również modyfikować żądania, zmieniając metody, dodając nagłówki lub ciało żądania. Warto również zwrócić uwagę na odpowiedź serwera, ponieważ może zawierać ważne informacje lub błędy.

## Zobacz także

- [Dokumentacja WiFiClient](https://www.arduino.cc/en/Reference/WiFiClient)
- [Arduino - Wprowadzenie do Internetu](https://www.arduino.cc/en/Guide/ArduinoWiFi101)
- [Biblioteka WiFiClient](https://www.arduino.cc/en/Reference/WiFiClientConstructor)