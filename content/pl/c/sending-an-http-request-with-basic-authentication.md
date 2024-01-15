---
title:                "Wysyłanie żądania http z podstawową autoryzacją."
html_title:           "C: Wysyłanie żądania http z podstawową autoryzacją."
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją."
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego 

W dzisiejszych czasach wiele aplikacji i stron internetowych wymaga uwierzytelnienia użytkownika za pomocą podstawowej autoryzacji HTTP. Jest to niezbędne do zapewnienia bezpieczeństwa i dostępu do poufnych danych. W tym artykule dowiecie się, dlaczego warto wiedzieć, jak wysyłać żądania HTTP z podstawową autoryzacją w języku C.

## Jak To Zrobić 

Aby wysłać żądanie HTTP z podstawową autoryzacją w języku C, należy wykonać kilka kroków:

1. Pierwszym krokiem jest zaimportowanie biblioteki `stdio.h` oraz `curl/curl.h`.

```
#include <stdio.h>
#include <curl/curl.h>
```

2. Następnie należy utworzyć strukturę `CURL` oraz zainicjować ją za pomocą funkcji `curl_easy_init()`.

```
CURL *curl;
curl = curl_easy_init();
```

3. Teraz można ustawić adres URL, do którego zostanie wysłane żądanie, za pomocą funkcji `curl_easy_setopt()`.

```
curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
```

4. Kolejnym ważnym krokiem jest ustawienie opcji `CURLOPT_HTTPAUTH` na wartość `CURLAUTH_BASIC`, co oznacza, że nasze żądanie będzie wysłane z podstawową autoryzacją HTTP.

```
curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
```

5. Aby dodać nazwę użytkownika i hasło, należy użyć funkcji `curl_easy_setopt()` z opcją `CURLOPT_USERPWD`.

```
curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");
```

6. Następnie możemy wykonać żądanie za pomocą funkcji `curl_easy_perform()`.

```
curl_easy_perform(curl);
```

Po wykonaniu tych kroków, wysłane zostanie żądanie HTTP z podstawową autoryzacją. Poniżej znajduje się kompletny przykład kodu:

```
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");
    curl_easy_perform(curl);
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

Aby wyświetlić powrót od serwera, można dodać do kodu funkcję `printf()`, która wydrukuje odpowiedź w konsoli.

## Deep Dive

Podczas wysyłania żądania HTTP z podstawową autoryzacją, warto zrozumieć, jakie są jej podstawowe elementy. Nazwa użytkownika i hasło są zazwyczaj wysyłane w formacie `nazwa_użytkownika:hasło`, a następnie są kodowane w odpowiedni sposób, aby uniknąć przesyłania ich w sposób jawny przez sieć.

Podstawowa autoryzacja odbywa się poprzez wysłanie nagłówka `Authorization` wraz z wartością `Basic` oraz zakodowaną nazwą użytkownika i hasłem. Serwer odbierając takie żądanie, dekoduje dane i sprawdza, czy są one poprawne.

## Zobacz Również

Jeśli chcesz pogłębić swoją wiedzę na temat wysyłania żądań HTTP z podstawową autoryzacją w języku C, polecamy zapoznać się z następującymi artykułami i dokumentacjami:

- [Dokumentacja biblioteki libcurl](https://curl.haxx.se/libcurl/c)
- [Strona oficjalna języka C](https://www.cplusplus.com/)
- [Wykorzystanie podstawowej autoryzacji w protokole HTTP](https://en.wikipedia.org/wiki/Basic_access_authentication)