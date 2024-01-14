---
title:                "Arduino: Przesyłanie żądania http z uwierzytelnianiem podstawowym"
simple_title:         "Przesyłanie żądania http z uwierzytelnianiem podstawowym"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele sytuacji, kiedy chcielibyśmy wysłać zapytanie HTTP wraz z podstawową autoryzacją z poziomu naszego projektu w Arduino. Może to być konieczne do komunikacji z zewnętrznymi serwisami, wykonywania zdalnych żądań lub różnego rodzaju integracji z internetowymi usługami. Dlatego ważne jest, aby umieć obsługiwać tego typu zapytania przy pomocy Arduino.

## Jak to zrobić

Aby wysłać zapytanie HTTP z wykorzystaniem podstawowej autoryzacji w Arduino, musimy użyć przewiązanej biblioteki "WiFiClient.h". Następnie możemy użyć funkcji "Basic" do ustawienia loginu i hasła w formacie "username:password". W następnym kroku należy pobrać klienta WiFi i ustawić adres oraz port serwera, do którego chcemy wysłać zapytanie. Następnie można zdefiniować żądanie metodą "GET" lub "POST" oraz podać potrzebne nagłówki. W końcu, gdy jesteśmy gotowi, możemy wysłać nasze żądanie przy pomocy metody "show" i odczytać odpowiedź poprzez metodę "readString". Oto przykładowy kod, który przedstawia to wyżej:

```
Arduino #include <WiFiClient.h>

char* server = "example.com";
int port = 80;

void setup() {
  Serial.begin(115200);

  WiFiClient client;
  if (!client.connect(server, port)) {
    Serial.println("connection failed");
    return;
  }

  client.print("GET / HTTP/1.0\r\n");
  client.print("Host: example.com\r\n");
  client.print("Authorization: Basic dXNlcm5hbWU6cGFzc3dvcmQ=\r\n");
  client.print("Connection: close\r\n\r\n");
  delay(100);

  while(client.available()) {
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }
}

void loop() {
  //
}
```

W powyższym przykładzie użytkownik to "username", a hasło to "password". Może to być kodowane w base64 lub inny wybrany przez nas sposób, pod warunkiem, że jest to zgodne ze standardem HTTP.

## Wnikliwa analiza

Wysyłanie zapytań HTTP z podstawową autoryzacją może wydawać się prostym zadaniem, ale istnieje kilka szczegółów, które warto zauważyć. Po pierwsze, ważne jest, aby znać poprawne sposoby kodowania logina i hasła oraz umieć wykorzystać odpowiednie nagłówki w zależności od używanego protokołu HTTP. Ważne jest również, aby upewnić się, że nasze żądanie zostało poprawnie wysłane i odbierać odpowiedź w sposób, który zapobiega błędom. Jeśli chcemy bardziej zaawansowanych funkcji, warto rozważyć użycie gotowych bibliotek do obsługi HTTP, które mogą ułatwić nam pracę.

## Zobacz też

- [ESP8266 Arduino - WiFiClient class](https://arduino-esp8266.readthedocs.io/en/2.5.0/esp8266wifi/client-examples.html)
- [Base64 encoding and decoding](https://www.arduino.cc/reference/en/language/functions/advanced-io/base64decode/)
- [Basic Access Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)