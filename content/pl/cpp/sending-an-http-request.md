---
title:                "C++: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w naszych programach musimy komunikować się z innymi serwisami lub aplikacjami, by pobierać dane lub wykonywać pewne operacje. W takich sytuacjach wykorzystujemy protokół HTTP i wysyłamy żądanie do serwera. W tym wpisie dowiecie się, jak wykorzystać język C++ do wysyłania takich żądań.

## Jak to zrobić

Tworzenie i wysyłanie żądań HTTP za pomocą C++ jest stosunkowo proste dzięki bibliotece `libcurl`. Wystarczy przeprowadzić kilka kroków:

1. Importowanie biblioteki `curl/curl.h` w naszym programie.
2. Stworzenie obszaru pamięci za pomocą funkcji `CURL *curl_easy_init()` i przypisanie do niego uchwytu.
3. Ustawienie opcji dla żądania, takich jak adres URL i metoda, za pomocą funkcji `curl_easy_setopt()`.
4. Wysłanie żądania przez wywołanie funkcji `curl_easy_perform()`.
5. Zwolnienie pamięci za pomocą funkcji `curl_easy_cleanup()`.

Aby uzyskać szczegółowe przykłady kodu i wyników, zobaczcie poniższe bloki kodu:

```C++
#include <curl/curl.h>

int main()
{
  // Inicjalizacja obszaru pamięci
  CURL *curl = curl_easy_init();
  // Ustawienia opcji żądania
  curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
  curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
  // Wysłanie żądania
  CURLcode response = curl_easy_perform(curl);
  // Zwolnienie pamięci
  curl_easy_cleanup(curl);
  return 0;
}
```

W powyższym przykładzie wysłaliśmy proste żądanie GET na stronę `www.example.com` i otrzymaliśmy kod odpowiedzi 200, co oznacza sukces.

## Deep Dive

Jeśli chcielibyśmy się zagłębić w temat wysyłania żądań HTTP przy użyciu C++, można wykorzystać wiele dodatkowych opcji i funkcji z biblioteki `libcurl`. Niektóre z nich to:

- Umożliwienie przesyłania danych z zapytaniem (np. formularz) poprzez funkcję `curl_easy_setopt()` z opcją `CURLOPT_POSTFIELDS`.
- Ustawienie nagłówków żądania za pomocą funkcji `curl_slist_append()`.
- Odbieranie i obsługa danych zwracanych przez serwer poprzez funkcję `curl_easy_setopt()` z opcją `CURLOPT_WRITEFUNCTION`.

Dzięki temu możemy dostosować nasze żądanie do konkretnych potrzeb i otrzymać bieżące informacje z serwera.

## See Also

Jeśli jesteście zainteresowani bardziej zaawansowanym wykorzystaniem biblioteki `libcurl`, zobaczcie poniższe przydatne linki:

- [Dokumentacja biblioteki libcurl](https://curl.haxx.se/libcurl/)
- [Przewodnik po wysyłaniu żądań HTTP w C++ przy użyciu biblioteki libcurl](https://www.baeldung.com/curl-libcurl-cpp)
- [Wysyłanie żądań HTTP w C++ - poradnik od Google Developers](https://developer.android.com/training/articles/perf-tips.html)

Zapraszamy również do dzielenia się swoimi doświadczeniami i pytaniami w komentarzach.