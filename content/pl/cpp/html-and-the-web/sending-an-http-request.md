---
title:                "Wysyłanie żądania HTTP"
aliases:
- /pl/cpp/sending-an-http-request/
date:                  2024-01-20T17:59:04.760061-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wysyłanie żądania HTTP polega na poproszeniu serwera o dane lub zasoby. Programiści robią to, aby komunikować się z aplikacjami internetowymi, pobierać dane, wysyłać formularze, i więcej.

## Jak to zrobić:
C++ nie ma wbudowanej obsługi HTTP, ale możesz użyć biblioteki, jak `CPR` dla prostoty:
```cpp
#include <cpr/cpr.h>
#include <iostream>

int main() {
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/get"});
    std::cout << r.text << std::endl; // wyświetla odpowiedź jako tekst

    // wysyłanie żądania POST
    r = cpr::Post(cpr::Url{"http://httpbin.org/post"},
                   cpr::Payload{{"key", "value"}});
    std::cout << r.text << std::endl;

    return 0;
}
```
Wynik powyższego kodu to tekstowe reprezentacje odpowiedzi serwera na żądania GET i POST.

## Deep Dive
W przeszłości programiści C++ musieli używać skomplikowanych bibliotek jak libcurl. `CPR` (C++ Requests) jest nowoczesnym, prostszym odpowiednikiem popularnego Pythona `requests`. Alternatywy to POCO i Boost.Beast (dla tych, co potrzebują czegoś na poziomie niższym).

Implementacja żądania HTTP w C++ wymaga obsługi sieci, formatowania zapytań oraz przetwarzania odpowiedzi. Biblioteki, jak `CPR`, zarządzają tym za Ciebie, pozwalając skupić się na logice biznesowej.

## Zobacz też
- [CPR GitHub Page](https://github.com/libcpr/cpr)
- [libcurl](https://curl.haxx.se/libcurl/)
- [POCO Libraries](https://pocoproject.org/)
- [Boost.Asio (w tym Boost.Beast)](https://www.boost.org/doc/libs/release/libs/beast/)
