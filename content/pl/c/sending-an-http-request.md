---
title:                "Wysyłanie żądania HTTP"
html_title:           "C: Wysyłanie żądania HTTP"
simple_title:         "Wysyłanie żądania HTTP"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu często potrzebujemy komunikować się z serwerami internetowymi, wysyłając do nich żądania i oczekując odpowiedzi. W języku C możemy wykorzystać bibliotekę `libcurl`, aby w prosty sposób wysyłać zapytania HTTP.

## Jak zacząć

Najpierw musimy zaimportować bibliotekę `libcurl` w naszym kodzie. Możemy to zrobić za pomocą dyrektywy `#include <curl/curl.h>`. Następnie możemy zdefiniować funkcję, która będzie wysyłać nasze żądanie. W tym przypadku będzie to funkcja o nazwie `send_request`.

```C
#include <curl/curl.h>

void send_request() {
    // Tutaj będziemy wysyłać żądanie
}
```

Teraz możemy stworzyć zmienną typu `CURL`, która będzie przechowywać nasze połączenie HTTP. Użyjemy funkcji `curl_easy_init()` aby ją zainicjować.

```C
CURL *curl = curl_easy_init();
```

Następnie ustawiamy URL serwera, do którego chcemy się połączyć, za pomocą funkcji `curl_easy_setopt()`. W tym przypadku będzie to http://example.com.

```C
curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
```

Teraz możemy wysłać nasze żądanie, używając funkcji `curl_easy_perform()`. W przypadku sukcesu, funkcja zwróci `CURLE_OK`.

```C
CURLcode res = curl_easy_perform(curl);
if (res != CURLE_OK) {
    // Obsłuż błąd
}
```

Po zakończeniu połączenia, musimy jeszcze zwolnić pamięć, używając funkcji `curl_easy_cleanup()`.
```C
curl_easy_cleanup(curl);
```

## Deep Dive

Wysyłanie prostej żądania HTTP za pomocą biblioteki `libcurl` jest łatwe, ale biblioteka ta oferuje również wiele innych funkcji. Na przykład możemy ustawić różne opcje dla naszego żądania, takie jak nagłówki, limity czasu czy dane do wysłania. Możemy również przetwarzać odpowiedź serwera, wydobywając z niej potrzebne nam informacje.

Każda z funkcji, takich jak `curl_easy_setopt()` czy `curl_easy_perform()`, ma wiele opcji dostępnych w dokumentacji `libcurl`. Możemy zapoznać się z nimi i dostosować wysyłane żądanie do naszych potrzeb.

## Zobacz też

- [Oficjalna dokumentacja biblioteki `libcurl`](https://curl.se/libcurl/)
- [Przykłady użycia w języku C](https://curl.se/libcurl/c/example.html)