---
title:                "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach coraz więcej urządzeń komunikuje się za pomocą protokołu HTTP. Oznacza to, że urządzenia takie jak Arduino mogą wysyłać zapytania i pobierać dane z Internetu. W tym artykule dowiesz się, jak wysyłać zapytania HTTP za pomocą Arduino.

## Jak to zrobić

Cały proces wysyłania zapytania HTTP za pomocą Arduino jest prosty i wymaga tylko kilku linii kodu. Pierwszym krokiem jest zdefiniowanie adresu URL, do którego chcemy wysłać zapytanie. Następnie musimy utworzyć obiekt klienta HTTP i użyć funkcji "GET" lub "PUT", w zależności od tego, czy chcemy pobierać dane czy też wysyłać je na serwer. Na koniec musimy wywołać funkcję "readString", która wyświetli odpowiedź od serwera.

```Arduino
#include <ESP8266WiFi.h>
#include <WiFiClient.h>
 
const char* ssid = "nazwa_sieci";
const char* password = "hasło_sieci";
 
const char* host = "adres_URL";
 
void setup() {
  Serial.begin(115200);
  Serial.println();
  Serial.print("Łączenie z ");
  Serial.println(ssid);
 
  WiFi.begin(ssid, password);
 
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("");
  Serial.println("Połączono z siecią Wi-Fi");
}
 
void loop() {
  WiFiClient client;
  const int httpPort = 80;
  if (!client.connect(host, httpPort)) {
    Serial.println("Nie można połączyć z serwerem HTTP");
    return;
  }
  
  client.print("GET / HTTP/1.1\r\n");
  client.print("Host: ");
  client.println(host);
  client.println("Connection: close");
  client.println();
 
  while (client.connected()) {
    String line = client.readStringUntil('\n');
    if (line == "\r") {
      Serial.println("Wysłano zapytanie");
      break;
    }
  }
  String line = client.readStringUntil('\n');
  Serial.println("Odpowiedź serwera:");
  Serial.println(line);
  Serial.println("Rozłączono");
 
  delay(5000);
}
```

Po uruchomieniu tego kodu Arduino będzie wysyłał zapytanie do podanego adresu URL i wyświetlał odpowiedź od serwera w monitorze szeregowym.

## Pogłębiona analiza

Wysyłanie zapytania HTTP nie jest trudnym zadaniem, ale warto poznać kilka ważnych aspektów, aby uniknąć problemów podczas programowania.

Po pierwsze, należy pamiętać o użyciu odpowiedniej biblioteki zależnie od wersji Arduino, którą się posiada. W naszym przykładzie używamy biblioteki "ESP8266WiFi" dla modułu ESP8266. W przypadku innych wersji Arduino, należy wybrać odpowiednią bibliotekę.

Kolejnym ważnym aspektem jest umiejętne budowanie adresu URL. Musi on zawierać protokół (http lub https), a także pełną ścieżkę dostępu do zasobu, na przykład "/index.html".

Ostatnim, ale bardzo ważnym aspektem jest analiza odpowiedzi serwera. Zwykle serwery wysyłają odpowiedź w wielu liniach, a ostatnia linia zakończona jest pustym wierszem. Należy więc użyć funkcji "readStringUntil('\n')" do odczytania całej odpowiedzi serwera.

## Zobacz również

- [Dokumentacja ESP8266WiFi](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
- [Lista bibliotek ESP8266](https://github.com/esp8266/Arduino#installing-with-boards-manager)
- [HTTP Requests w Arduino używając biblioteki ESP8266WiFi](https://circuits4you.com/2018/02/21/