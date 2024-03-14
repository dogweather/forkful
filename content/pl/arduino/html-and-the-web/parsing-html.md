---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:44.996071-07:00
description: "Przetwarzanie HTML w projektach Arduino polega na ekstrakcji informacji\
  \ ze stron internetowych. Programi\u015Bci robi\u0105 to, aby umo\u017Cliwi\u0107\
  \ ich urz\u0105dzeniom\u2026"
lastmod: '2024-03-13T22:44:35.670202-06:00'
model: gpt-4-0125-preview
summary: "Przetwarzanie HTML w projektach Arduino polega na ekstrakcji informacji\
  \ ze stron internetowych. Programi\u015Bci robi\u0105 to, aby umo\u017Cliwi\u0107\
  \ ich urz\u0105dzeniom\u2026"
title: "Analiza sk\u0142adniowa HTML"
---

{{< edit_this_page >}}

## Co i dlaczego?

Przetwarzanie HTML w projektach Arduino polega na ekstrakcji informacji ze stron internetowych. Programiści robią to, aby umożliwić ich urządzeniom Arduino interakcję z Internetem, zbierając dane ze stron internetowych na różne cele, począwszy od automatyzacji domowej aż po monitoring środowiskowy.

## Jak to zrobić:

Przetwarzanie HTML na Arduino zwykle wymaga bibliotek o minimalnym rozmiarze ze względu na ograniczone zasoby urządzenia. Popularnym wyborem do scrapingu i przetwarzania stron internetowych jest użycie bibliotek `ESP8266HTTPClient` i `ESP8266WiFi` dla ESP8266, lub ich odpowiedników dla ESP32, biorąc pod uwagę ich wbudowane wsparcie dla możliwości Wi-Fi i protokołów HTTP. Oto podstawowy przykład pobierania i przetwarzania HTML, zakładając, że pracujesz na ESP8266 lub ESP32:

Najpierw dołącz konieczne biblioteki:
```cpp
#include <ESP8266WiFi.h> // Dla ESP8266
#include <ESP8266HTTPClient.h>
#include <WiFiClient.h>
// Użyj analogicznych bibliotek ESP32, jeśli korzystasz z ESP32

const char* ssid = "twojeSSID";
const char* password = "twojeHASŁO";
```

Połącz się z siecią Wi-Fi:
```cpp
void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);

    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("Łączenie...");
    }
}
```

Wyślij żądanie HTTP i przetwórz prosty fragment HTML:
```cpp
void loop() {
    if (WiFi.status() == WL_CONNECTED) { // Sprawdź status połączenia z Wi-Fi
        HTTPClient http;  // Zadeklaruj obiekt klasy HTTPClient

        http.begin("http://example.com");  // Określ cel żądania
        int httpCode = http.GET();  // Wyślij żądanie

        if (httpCode > 0) { // Sprawdź kod powrotny
            String payload = http.getString();   // Pobierz treść odpowiedzi na żądanie
            Serial.println(payload);             // Wydrukuj odpowiedź

            // Przetwórz konkretną część, np. ekstrahując tytuł z treści
            int titleStart = payload.indexOf("<title>") + 7; // +7, żeby przesunąć za znacznik "<title>"
            int titleEnd = payload.indexOf("</title>", titleStart);
            String pageTitle = payload.substring(titleStart, titleEnd);

            Serial.print("Tytuł strony: ");
            Serial.println(pageTitle);
        }

        http.end();   // Zamknij połączenie
    }

    delay(10000); // Wykonuj żądanie co 10 sekund
}
```

Przykładowe wyjście (zakładając, że http://example.com ma prostą strukturę HTML):
```
Łączenie...
...
Tytuł strony: Przykładowa domena
```

Ten przykład demonstruje pobieranie strony HTML i ekstrakcję zawartości znacznika `<title>`. Do przetwarzania bardziej skomplikowanego HTML, rozważ użycie wyrażeń regularnych (z ostrożnością ze względu na ograniczenia pamięci) lub funkcji manipulacji ciągami, aby poruszać się po strukturze HTML. Zaawansowane przetwarzanie może wymagać bardziej zaawansowanych podejść, w tym niestandardowych algorytmów przetwarzania dostosowanych do konkretnej struktury HTML, z którą masz do czynienia, ponieważ standardowe środowisko Arduino nie obejmuje wbudowanej biblioteki do przetwarzania HTML.
