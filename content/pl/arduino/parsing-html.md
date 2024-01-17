---
title:                "Analiza składni HTML"
html_title:           "Arduino: Analiza składni HTML"
simple_title:         "Analiza składni HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Co&dlaczego?
Parsing HTML to proces, który pozwala programistom na analizowanie i przetwarzanie kodu źródłowego stron internetowych. Wykorzystuje się go w celu pobierania i wyświetlania zawartości witryn, wyodrębniania określonych informacji lub weryfikacji poprawności formatowania. Jest to niezbędne narzędzie dla twórców aplikacji internetowych i automatycznych robotów przeglądających sieci.

## Jak to zrobić:
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

void setup() {
  Serial.begin(115200); // inicjacja portu szeregowego
  WiFi.begin("WIFI nazwa sieci", "WIFI hasło"); // połączenie z siecią 
  while (WiFi.status() != WL_CONNECTED) { //pętla oczekująca 
    delay(500);
    Serial.println("Connecting to WiFi..");
  }
}

void loop() {
  if (WiFi.status() == WL_CONNECTED) {
    HTTPClient http; //deklaracja obiektu HTTPClient
    http.begin("http://blabla.com/"); //adres żądanej strony
    int httpCode = http.GET(); //wysłanie żądania GET
    if (httpCode > 0) {
      String payload = http.getString(); //przypisanie odpowiedzi do stringa
      Serial.println(payload); //wyświetlenie odpowiedzi w serial monitorze
    }
    http.end(); //zakończenie sesji HTTP
    
  }
  delay(5000); // opóźnienie między kolejnymi żądaniami
}
```

## W zagłębienie:
Parsing HTML istniał już w latach 90-tych, a jego popularność zwiększyła się wraz z rozwojem aplikacji internetowych. W alternatywnych podejściach do przetwarzania kodu HTML wykorzystuje się również inne narzędzia, takie jak XPath czy BeautifulSoup. W przypadku implementacji w Arduino, należy jednak uważać na zużycie pamięci i czas wykonania, ponieważ analiza kodu źródłowego może wymagać dużej ilości zasobów.

## Zobacz też:
- Dokumentacja Arduino: https://www.arduino.cc/reference/en/libraries/webservicesclient/
- Instrukcje tworzenia aplikacji internetowych: https://developer.mozilla.org/pl/docs/Web/API/Document_Object_Model
- Poradnik dla początkujących w programowaniu Arduino: https://www.arduino.cc/en/Guide/HomePage