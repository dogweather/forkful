---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Konwersja Stringów na Małe Litery w C++: Jak to Zrobić
Napiszemy dzisiaj o konwersji stringów na małe litery w C++. Jest to wystarczająco częste, aby zatytułować ten wpis "Jak to zrobić".

## Co & Dlaczego?
Konwersja stringa na małe litery polega na zamianie wszystkich wielkich liter w stringu na małe. Programiści robią to często, aby ułatwić sobie porównywanie i sortowanie stringów.

## Jak to Zrobić:
Wykorzystamy wbudowaną w C++ funkcję `tolower()`. Oto prosty przykład: 

```C++
#include <cctype>
#include <algorithm>

int main() {
    std::string str_upper("WITAJ, ŚWIECIE!");
    std::transform(str_upper.begin(), str_upper.end(), str_upper.begin(), ::tolower);
    std::cout << str_upper;
}
```

Wyjście:  `witaj, świecie!`

## Szczegółowe Omówienie
Konwersja stringów na małe litery to powszechna operacja, szczególnie w kontekście przetwarzania tekstu i analizy danych. 

1. **Kontekst Historyczny**: Początkowo, funkcje konwersji przypadków były częścią większości bibliotek języków programowania, z koniecznością obsługi różnych standardów kodowania. Wraz z ewolucją Unicode, funkcje te stały się bardziej złożone i zróżnicowane.

2. **Alternatywy**: Jest wiele innych metod konwersji stringa na małe litery. Możemy również użyć pętli for, iterując przez każdy znak i korzystając z funkcji `tolower()`, czy skorzystać z `boost::algorithm::to_lower(str)` z biblioteki Boost.

3. **Szczegóły Implementacji**: Funkcja `tolower()` konwertuje każdy znak na małą literę (jeśli jest to litera). Najważniejsze jest, aby pamiętać o poprawnym obszarze kodowania, szczególnie dla non-ASCII znaków.

## Zobacz Również
Więcej na ten temat można znaleźć, przechodząc do tych źródeł:
1. ["tolower" na cppreference.com](https://en.cppreference.com/w/cpp/string/byte/tolower)
2. [Lowercasing strings with Boost](https://www.boost.org/doc/libs/1_73_0/doc/html/boost/algorithm/to_lower.html)
3. [Unicode Case Mapping](https://unicode.org/faq/casemap_charprop.html)