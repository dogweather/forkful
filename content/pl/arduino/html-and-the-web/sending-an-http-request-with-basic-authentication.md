---
title:                "Wysyłanie zapytania http z podstawową autoryzacją"
aliases: - /pl/arduino/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:02.259689-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie zapytania http z podstawową autoryzacją"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem pozwala Arduino komunikować się z zabezpieczonymi serwerami. Programiści używają tej metody, aby wymieniać dane z API lub stronami internetowymi, które wymagają loginu i hasła.

## Jak to zrobić:

Instalacja biblioteki do zarządzania połączeniami WiFi i HTTP jest pierwszym krokiem. Użyjemy `WiFiNINA.h` i `HTTPClient.h`. Przykład kodu:

```Arduino
#include <WiFiNINA.h>
#include <HTTPClient.h>

const char* ssid = "Twoja-Siec-WiFi";
const char* password = "TwojeHaslo";
const char* serverName = "http://twoja.domena.com/ścieżka";
const char* httpUser = "użytkownik";
const char* httpPassword = "hasło";

WiFiClient wiFiClient;
HTTPClient httpClient;

void setup() {
  Serial.begin(115200);

  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("Połączono z WiFi");
  
  httpClient.begin(wiFiClient, serverName);
  httpClient.setAuthorization(httpUser, httpPassword);
  
  int httpResponseCode = httpClient.GET();
  
  if (httpResponseCode > 0) {
    String payload = httpClient.getString();
    Serial.println(httpResponseCode);
    Serial.println(payload);
  } else {
    Serial.print("Błąd: ");
    Serial.println(httpResponseCode);
  }
  
  httpClient.end();
}

void loop() {
  // Nie ma potrzeby wykonywania żądania w pętli bez przerwy
}
```

Wyjście (sample output) będzie wyglądać jako ciąg informacji zwrotnych od serwera, w tym kod odpowiedzi, który powie nam, czy żądanie się powiodło.

## Głębsze spojrzenie

O wykorzystaniu HTTP już w latach 90 XX wieku decydowała prostota i uniwersalność. W Arduino, do komunikacji z serwerami, kluczowe są biblioteki, takie jak `WiFiNINA.h`, która obsługuje standardy sieci WiFi, oraz `HTTPClient.h`, pozwalająca na tworzenie żądań HTTP.

Istnieją też alternatywy, jak `ESP8266HTTPClient.h` dla modułów ESP8266, czy rozwiązania zewnętrznych usług, np. IFTTT lub MQTT dla innych typów zadań IoT. Parametry uwierzytelnienia w 'basic authentication' są kodowane w base64, ale to załatwia za nas biblioteka 'HTTPClient.h'.

## Zobacz także

Dokumentacja i przykłady:
- [Biblioteka WiFiNINA](https://www.arduino.cc/en/Reference/WiFiNINA)
- [Biblioteka HTTPClient](https://www.arduino.cc/reference/en/libraries/httpclient/)

Wiedza o kodowaniu w base64 i uwierzytelnianiu HTTP:
- [Basic access authentication (Wikipedia)](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)

Arduino i IoT:
- [Oficjalne forum Arduino](https://forum.arduino.cc/)
- [Przykłady projektów IoT na Arduino](https://create.arduino.cc/projecthub?by=arduino)
