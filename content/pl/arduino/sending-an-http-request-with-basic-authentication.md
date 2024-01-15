---
title:                "Wysyłanie żądania http z uwierzytelnieniem podstawowym"
html_title:           "Arduino: Wysyłanie żądania http z uwierzytelnieniem podstawowym"
simple_title:         "Wysyłanie żądania http z uwierzytelnieniem podstawowym"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie żądania HTTP z uwierzytelnieniem podstawowym może być przydatne, gdy chcemy uzyskać dostęp do chronionego zasobu lub wykonania określonej czynności na zdalnym serwerze.

## Jak to zrobić

```Arduino
#include <WiFiClientSecure.h> // dołącz bibliotekę WiFiClientSecure

char ssid[] = "nazwa_sieci"; // wprowadź nazwę sieci WiFi
char pass[] = "hasło"; // wprowadź hasło sieci WiFi
char host[] = "adres_url"; // wprowadź adres URL dla żądania HTTP
int port = 443; // podstawowy port dla HTTPS

WiFiClientSecure client; // inicjalizuj obiekt klienta WiFiClientSecure

void setup() {
  Serial.begin(9600); // inicjalizuj port szeregowy
  
  WiFi.begin(ssid, pass); // połącz z siecią WiFi
  Serial.println("Connecting to WiFi...");
  
  while (WiFi.status() != WL_CONNECTED) { // oczekuj na połączenie
    delay(500);
    Serial.print(".");
  }

  Serial.println("Connected!"); // jeśli połączenie nawiązane, wyświetl komunikat

  // Wykonaj żądanie HTTP do serwera
  Serial.println("Making HTTPS request to server...");
  if (client.connect(host, port)) { // połącz się z serwerem
    client.print("GET / HTTP/1.1\r\n"); // ustaw metodę, ścieżkę i wersję protokołu
    client.print("Host: ");
    client.print(host);
    client.print("\r\n");
    client.print("Authorization: Basic YWxhZGRpbjpvcGVuc2VzYW1l\r\n"); // dodaj nagłówek uwierzytelniania
    client.print("\r\n");
  }
}

void loop() {
  if (client.connected() && !client.available()) { // sprawdź czy połączenie jest aktywne i nie ma już dostępnych danych
    Serial.println("No more data!"); // jeśli nie ma już danych, wyświetl komunikat
    client.stop(); // zamknij połączenie
    while(true); // zatrzymaj pętlę
  }
  
  while(client.available()) { // dopóki są dostępne dane
    char c = client.read(); // odczytaj znak
    Serial.print(c); // wyświetl go na monitorze szeregowym
  }
}

```

### Przykładowy wynik

Po wykonaniu żądania HTTP z basic authentication, otrzymamy odpowiedź serwera zawierającą informacje lub wykonującą określoną akcję.

```
HTTP/1.1 200 OK
Date: Sun, 22 Aug 2021 17:00:00 GMT
Server: Apache
Content-Type: text/html
Content-Length: 15

Hello from server
```

## Deep Dive

Podczas wysyłania żądania HTTP z uwierzytelnieniem podstawowym, należy wysłać nagłówek "Authorization" zawierający kodowania Base64 nazwy użytkownika i hasła, oddzielone dwukropkiem (np. "Authorization: Basic YWxhZGRpbjpvcGVuc2VzYW1l").

W przypadku uwierzytelniania podstawowego, informacje te są wysyłane w otwartym tekście i nie stanowią idealnego sposobu zabezpieczenia naszych danych. W przyszłości warto rozważyć korzystanie z innego rodzaju uwierzytelniania, takiego jak uwierzytelnianie z użyciem tokenu.

## Zobacz także

- [Podstawowe kody odpowiedzi serwera HTTP](https://developer.mozilla.org/pl/docs/Web/HTTP/Status)
- [Dokumentacja biblioteki WiFiClientSecure](https://www.arduino.cc/en/Reference/WiFiClientSecure)
- [Dokumentacja protokołu HTTP](https://developer.mozilla.org/pl/docs/Web/HTTP)