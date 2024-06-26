---
date: 2024-01-20 17:59:04.760061-07:00
description: "Jak to zrobi\u0107: C++ nie ma wbudowanej obs\u0142ugi HTTP, ale mo\u017C\
  esz u\u017Cy\u0107 biblioteki, jak `CPR` dla prostoty."
lastmod: '2024-03-13T22:44:35.709673-06:00'
model: gpt-4-1106-preview
summary: "C++ nie ma wbudowanej obs\u0142ugi HTTP, ale mo\u017Cesz u\u017Cy\u0107\
  \ biblioteki, jak `CPR` dla prostoty."
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

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
