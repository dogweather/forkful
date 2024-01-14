---
title:                "Arduino: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Arduino jest popularnym narzędziem do tworzenia projektów związanych z elektroniką i robotyką. Jednym z jego wielu możliwości jest pobieranie stron internetowych. Dzięki temu funkcjonalności możemy wykorzystać nasze urządzenie do dostępu do różnych informacji z internetu, co może być bardzo przydatne w wielu projektach.

## Jak To Zrobić

Poniżej przedstawiam przykładowy kod w języku Arduino, który pobiera zawartość strony internetowej i wyświetla ją na monitorze szeregowym. Za pomocą tego kodu będziemy mogli pobrać np. aktualną pogodę lub informacje z serwisu społecznościowego.

```Arduino
#include <SPI.h>
#include <WiFiClient.h>
#include <ESP8266WiFi.h>

char ssid[] = "YourNetworkName";     // nazwa sieci Wi-Fi
char password[] = "YourNetworkPass"; // hasło do sieci Wi-Fi

IPAddress server(216,58,213,14);     // adres IP strony internetowej (Google w tym przypadku)

WiFiClient client;

void setup()
{
  Serial.begin(115200);               // inicjalizacja monitora szeregowego
  delay(100);

  WiFi.begin(ssid, password);         // łaczenie z siecią Wi-Fi

  while (WiFi.status() != WL_CONNECTED) {  // oczekiwanie na połączenie
    delay(500);
    Serial.println("Connecting to WiFi..");
  }

  Serial.println("Connected to the WiFi network");

  if (client.connect(server, 80)) {   // nawiązanie połączenia z serwerem
    Serial.println("Connected to server");
    client.println("GET / HTTP/1.1"); // zapytanie o strone główną
    client.println("Host: www.google.com");
    client.println("Connection: close");
    client.println();
  }
}

void loop()
{
  while (client.available()) {      // sprawdzanie dostępności danych z serwera
    char c = client.read();
    Serial.print(c);                 // wyświetlanie danych na monitorze szeregowym
  }

  if (!client.connected()) {
    Serial.println();
    Serial.println("Disconnecting from server");
    client.stop();                   // zakończenie połączenia
    while (true);
  }
}

```

Po skompilowaniu i wgraniu kodu na płytkę Arduino oraz połączeniu się z siecią Wi-Fi otrzymamy na monitorze szeregowym kod źródłowy strony głównej Google.

## Deep Dive

Pobieranie stron internetowych za pomocą Arduino wymaga użycia protokołu HTTP. Układ ESP8266 posiada wbudowane funkcje, które umożliwiają nawiązywanie połączeń przez ten protokół oraz odczytywanie i zapisywanie danych. W przypadku bardziej zaawansowanych projektów, warto również poznać protokół HTTPS, który służy do bezpiecznej transmisji danych za pomocą szyfrowania SSL.

## Zobacz także

- [Dokumentacja biblioteki WiFiClient](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/client-examples.html#wificlient)
- [Tutorial: Pobieranie danych z API za pomocą Arduino](https://create.arduino.cc/projecthub/makuna/weather-station-using-wemos-d1-mini-pro-d1ff69?f=1)