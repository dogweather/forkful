---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP to proces, w którym twoja aplikacja komunikuje się z serwerem, wysyłając żądanie do pobrania, wysłania lub zmiany danych. Programiści robią to, aby umożliwić ich aplikacjom korzystanie z usług, takich jak API, czy też interakcje z innymi serwisami w sieci.

## Jak to zrobić:

Wykorzystamy do tego bibliotekę `cpr` (C++ Requests). Poniżej znajduje się przykładowy kod:

```C++
#include <cpr/cpr.h>
#include <iostream>

int main() {
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/get"});
    std::cout << r.text << std::endl;  // prints HTML of the webpage
    return 0;
}
```
Jeśli wszystko jest skonfigurowane prawidłowo, na wyjściu powinny pojawiać się dane HTML strony.

## Głębsze spojrzenie

Wysyłanie żądań HTTP to praktyka, którą programiści stosują już od powstania internetu. Historia tej techniki sięga lat 90-tych XX wieku, kiedy to po raz pierwszy zaczęto stosować protokół HTTP do komunikacji między klientem a serwerem.

Jedną z alternatyw dla wysyłania żądań HTTP w C++ jest wykorzystanie biblioteki `libcurl`. Jednak `cpr` jest bardziej przyjazny dla C++, przez co często jest preferowany przez programistów tego języka.

Kiedy wysyłasz żądanie HTTP, twoja aplikacja tworzy połączenie z serwerem, wysyła żądanie a następnie czeka na odpowiedź. Wszystkie te szczegóły są ukryte za sceną, dzięki wysokopoziomowym bibliotekom jak `cpr`.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o tym, jak wysyłać żądania HTTP w C++, zobacz poniższe linki do dodatkowych zasobów:

1. [CPR Github Repo](https://github.com/whoshuu/cpr) – repozytorium GitHub projektu cpr.
2. [C++ HTTP libraries](https://cpp.libhunt.com/categories/1253-http) – lista bibliotek HTTP dla C++.
3. [libcurl C++ Guide](https://curl.se/libcurl/c/libcurl-tutorial.html) – przewodnik po bibliotece libcurl w C++.