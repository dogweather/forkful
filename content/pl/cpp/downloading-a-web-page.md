---
title:                "C++: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele powodów, dla których możesz chcieć pobrać stronę internetową za pomocą programowania w języku C++. Może to być konieczność automatyzacji procesów, żądanie danych lub sprawdzenie zawartości danej strony.

## Jak to zrobić

Aby pobrać stronę internetową za pomocą C++, potrzebujemy narzędzia do wysyłania żądania HTTP i przetwarzać otrzymaną odpowiedź. Jednym z popularnych wyborów jest biblioteka `libcurl`, która zapewnia bogate API do wykonywania takich operacji.

Przykładowy kod wykorzystujący `libcurl` wygląda następująco:

```C++
// importujemy bibliotekę
#include <curl/curl.h>

// funkcja, która zostanie wywołana po otrzymaniu odpowiedzi
size_t writeCallback(char* contents, size_t size, size_t nmemb, void* userp) {
  ((std::string*)userp)->append((char*)contents, size * nmemb);
  return size * nmemb;
}

int main() {
  // inicjalizujemy zmienną do przechowywania danych z odpowiedzi
  std::string response;

  // inicjalizujemy zmienną cURL
  CURL* curl = curl_easy_init();

  if (curl) {
    // ustawiamy adres URL strony do pobrania
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/");

    // ustawiamy funkcję callback do przetworzenia odpowiedzi
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeCallback);

    // przekazujemy adres do zmiennej, w której będą przechowywane dane
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    // wykonujemy żądanie HTTP
    CURLcode result = curl_easy_perform(curl);

    // sprawdzamy, czy żądanie wykonane zostało poprawnie
    if (result == CURLE_OK) {
      std::cout << response << std::endl; // wypisujemy zawartość odpowiedzi
    }

    // zwalniamy zasoby
    curl_easy_cleanup(curl);
  }

  return 0;
}
```

Po wykonaniu powyższego kodu, w konsoli powinna pojawić się zawartość pobranej strony internetowej.

## Głębszy przegląd

Jeśli chcesz dowiedzieć się więcej na temat pobierania stron internetowych za pomocą C++, istnieje wiele dodatkowych narzędzi i bibliotek, które mogą Ci pomóc w tym zadaniu. Należą do nich między innymi `Boost C++ libraries` oraz `Asio C++ library`.

Oprócz tego, warto zwrócić uwagę na zagadnienia takie jak obsługa błędów, parsowanie danych oraz zabezpieczenia przed atakami typu SQL injection.

## Zobacz także

- [Dokumentacja libcurl](https://curl.haxx.se/libcurl/)
- [Boost C++ libraries](https://www.boost.org/)
- [Asio C++ library](https://think-async.com/Asio/)
- [SQL injection](https://owasp.org/www-community/attacks/SQL_Injection)