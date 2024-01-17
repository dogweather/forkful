---
title:                "Pobieranie strony internetowej"
html_title:           "Arduino: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Co i dlaczego?

Pobieranie strony internetowej to proces pobierania zawartości witryny internetowej z internetu na lokalny komputer lub urządzenie. Programiści często pobierają strony internetowe, aby uzyskać dostęp do danych, które mogą być wykorzystane do analizy lub wyświetlenia użytkownikom.

# Jak to zrobić:

Arduino może być używane do pobierania stron internetowych przy użyciu Ethernet Shield lub WiFi Shield. Poniżej znajdziesz przykładowy kod oraz wynik.

## Kod:
```
#include <WiFi.h> //dołączenie biblioteki WiFi

//Ustawienie danych sieciowych WiFi
const char* SSID = "nazwa sieci";
const char* password = "hasło";

WiFiClient client;

void setup() {
  Serial.begin(115200); //inicjalizacja komunikacji szeregowej
  WiFi.begin(SSID, password); //łączenie z siecią WiFi
  while (WiFi.status() != WL_CONNECTED) { //czekanie na połączenie z siecią
    delay(500);
    Serial.print(".");
  }
  
  Serial.println("WiFi Connected!"); //połączenie nawiązane, wypisanie w konsoli

  //Pobieranie zawartości strony
  Serial.println("Downloading web page...");
  client.println("GET /index.html HTTP/1.1");
  client.println("Host: www.example.com");
  client.println("Connection: close");
  client.println();
}

void loop() {
  while (client.available()) { //sprawdzanie dostępności danych do odczytu
    String line = client.readStringUntil('\r'); //odczytanie linii tekstu
    Serial.print(line); //wyświetlenie odczytanej linii w konsoli
  }

  if (!client.connected()) { //sprawdzanie, czy połączenie zostało zamknięte
    Serial.println();
    Serial.println("Web page downloaded!");
    client.stop(); //zamknięcie połączenia
    while (true); //pętla nieskończona
  }
}
```

## Wynik w konsoli:
```
WiFi Connected!
Downloading web page...
HTTP/1.1 200 OK
Server: nginx/1.14.0 (Ubuntu)
Date: Tue, 15 Jun 2021 18:30:05 GMT
Content-Type: text/html
Content-Length: 234
Connection: close
Last-Modified: Tue, 08 Jun 2021 12:15:39 GMT
ETag: "60c02a1b-ea"
Accept-Ranges: bytes
[treść strony internetowej]
Web page downloaded!
```

# Głębsze zanurzenie:

Pobieranie stron internetowych jest popularną praktyką w dziedzinie programowania, szczególnie w przypadku analizy danych lub tworzenia aplikacji internetowych. Alternatywą dla korzystania z Arduino do pobierania stron internetowych jest użycie bibliotek lub frameworków programistycznych dostępnych w językach takich jak Python czy Java. Implementacja pobierania stron internetowych na Arduino wymaga podłączenia odpowiedniego shielda i napisania odpowiedniego kodu, jednak nie jest to trudne ani skomplikowane.

# Zobacz również:

- [Getting Started with Ethernet Shield for Arduino](https://github.com/arduino-libraries/Ethernet)
- [WiFi Library for Arduino](https://www.arduino.cc/en/Reference/WiFi)
- [Python Requests library for web crawling and scraping](https://requests.readthedocs.io/en/master/)