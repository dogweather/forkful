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

## Dlaczego

Każdy z nas lubi mieć wolność w wyborze i swobodę w działaniu. W dzisiejszych czasach żyjemy w erze technologii, gdzie dostęp do informacji jest niezwykle ważny. Arduino pozwala nam na pobieranie stron internetowych bez konieczności siedzenia przed komputerem.

## Jak To Zrobić

README: Funkcja ```WiFiClient client;```, służy do nawiązania połączenia z serwerem. Aby pobrać stronę internetową, wpisujemy adres URL w nawiasy funkcji ```client.connect()```, następnie wykorzystujemy pętlę ```while()``` do zapisania odpowiedzi z serwera.

```Arduino
#include <WiFi.h>

WiFiClient client;

void setup() {
  Serial.begin(115200);
  Serial.println();
  Serial.print("Connecting to Wi-Fi");
  WiFi.begin(SSID, PASSWORD);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println();
  Serial.print("Connected to Wi-Fi, IP address: ");
  Serial.println(WiFi.localIP());
}

void loop() {
  Serial.println("Making HTTP request: ");
  client.connect("www.example.com", 80);
  client.println("GET /index.html HTTP/1.0");
  client.println();

  while (client.available()) {
    String line = client.readStringUntil('\n');
    Serial.println(line);
  }
  delay(5000);
}
```

Output:

```
Connecting to Wi-Fi......
Connected to Wi-Fi, IP address: 192.168.1.1005
Making HTTP request: 
HTTP/1.1 200 OK
Date: Thu, 01 Jul 2021 12:00:00 GMT
Server: Apache/2.4.29 (Ubuntu)
Content-Length: 125
Content-Type: text/html

<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
... // HTML code from the website
</head>
<body>
</body>
</html>
```

## Deep Dive

Aby uzyskać dostęp do wszystkich elementów strony, możemy użyć odpowiednich funkcji do przetwarzania odpowiedzi serwera. Na przykład, jeśli chcielibyśmy uzyskać tylko treść strony, możemy wykorzystać funkcję ```client.find()```. Aby przetworzyć dane w odpowiednio sformatowany tekst, możemy użyć funkcji ```client.readStringUntil()```.

Funkcje ```client``` udostępniają nam wiele sposobów na analizowanie i przetwarzanie pobranych danych. Możemy również korzystać z dodatkowych bibliotek Arduino, takich jak ```HTMLParser```, aby ułatwić nam pracę z danymi HTML.

## Zobacz także

- [Dokumentacja Arduino o ```WiFi``` bibliotece](https://www.arduino.cc/en/Reference/WiFi)
- [Przykładowy kod pobierania strony internetowej na platformie Arduino](https://randomnerdtutorials.com/esp32-web-server-arduino-ide/)
- [Strona internetowa z przydatnymi funkcjami Arduino do pobierania wybranych elementów ze strony](http://www.botletics.com/esp32-http-get-post-arduino/)