---
title:                "Przesyłanie żądania http z podstawową autoryzacją"
html_title:           "C++: Przesyłanie żądania http z podstawową autoryzacją"
simple_title:         "Przesyłanie żądania http z podstawową autoryzacją"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie żądania HTTP z podstawową autoryzacją jest ważne, gdy chcemy zabezpieczyć nasze dane i ograniczyć dostęp do nich tylko dla wybranych użytkowników. Jest to często stosowane w przypadku aplikacji internetowych, gdzie dostęp do niektórych zasobów powinien być ograniczony tylko do autoryzowanych użytkowników.

## Jak to zrobić

```C++ 
#include <iostream>
#include <curl/curl.h>
 
int main()
{
  CURL *curl;
  CURLcode res;
 
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_USERNAME, "username"); // Ustawiamy nazwę użytkownika
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "password"); // Ustawiamy hasło
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com"); // Ustawiamy adres URL żądania
    res = curl_easy_perform(curl);
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

**Wynik:**

Jeśli żądanie zostanie wysłane prawidłowo, otrzymamy odpowiedź z serwera. W przypadku błędnych danych autoryzacyjnych, otrzymamy odpowiedni komunikat o błędzie.

## Deep Dive

Wysyłanie żądania HTTP z podstawową autoryzacją polega na dodaniu nagłówka "Authorization" do żądania. Nagłówek ten zawiera dane autoryzacyjne w postaci "username:password", które są kodowane w formacie Base64. Serwer następnie sprawdza, czy otrzymane dane są zgodne z tymi, które znajdują się w jego bazie danych. Jeśli tak, udziela dostępu do chronionych zasobów.

W przypadku, gdy dane autoryzacyjne są przesyłane w formie otwartej, czyli bez kodowania, są one narażone na przechwycenie przez osoby trzecie i mogą stanowić zagrożenie dla bezpieczeństwa aplikacji. Dlatego też, stosowanie podstawowej autoryzacji wymaga zachowania ostrożności i regularnej zmiany hasła.

## Zobacz także

- [Dokumentacja CURL](https://curl.se/libcurl/c/CURLOPT_USERNAME.html)
- [Poradnik programowania w C++](https://www.udemy.com/course/pentesting-in-auth-cryptography-passwords/)