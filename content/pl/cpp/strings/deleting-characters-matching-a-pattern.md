---
date: 2024-01-20 17:41:48.198353-07:00
description: "How to: (Jak to zrobi\u0107:) Usuwanie pasuj\u0105cych znak\xF3w jest\
  \ tak stare jak same wyra\u017Cenia regularne, kt\xF3re pojawi\u0142y si\u0119 w\
  \ latach 50. XX wieku. S\u0105 r\xF3\u017Cne\u2026"
lastmod: '2024-04-05T22:50:50.030780-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Usuwanie pasuj\u0105cych znak\xF3w jest tak stare\
  \ jak same wyra\u017Cenia regularne, kt\xF3re pojawi\u0142y si\u0119 w latach 50."
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

## How to: (Jak to zrobić:)
```C++
#include <iostream>
#include <string>
#include <regex>

std::string delete_matching_chars(std::string str, const std::string& pattern) {
    std::regex reg(pattern);
    return std::regex_replace(str, reg, "");
}

int main() {
    std::string data = "Cześć! Moje nr telefonu to 123-456-789, a email to example@domain.com";
    std::string pattern = "\\d"; // Usunięcie wszystkich cyfr
    std::string result = delete_matching_chars(data, pattern);
    std::cout << result << std::endl; // Output: Cześć! Moje nr telefonu to ---, a email to example@domain.com
    return 0;
}
```

## Deep Dive (Wgłębiając się)
Usuwanie pasujących znaków jest tak stare jak same wyrażenia regularne, które pojawiły się w latach 50. XX wieku. Są różne sposoby, np. iteracja po każdym znaku i sprawdzenie czy pasuje do wzoru. Wyrażenia regularne to jednak szybsze i bardziej elastyczne rozwiązanie.

Wiele języków ma własne wersje wyrażeń regularnych, jednak C++ stosuje bibliotekę `<regex>` obecną od C++11. Wcześniej używano zewnętrznych bibliotek, jak Boost.Regex. Alternatywą jest też ręczne tworzenie funkcji do przeszukiwania i zmiany danych, co może być szybsze dla prostych wzorców, lecz znacząco mniej czytelne i trudne w utrzymaniu.

## See Also (Zobacz również)
- [cppreference.com: std::regex](https://en.cppreference.com/w/cpp/regex)
- [Regular Expressions in C++ (cpluplus.com)](http://www.cplusplus.com/reference/regex/)
- [Boost.Regex documentation](https://www.boost.org/doc/libs/1_75_0/libs/regex/doc/html/index.html)
