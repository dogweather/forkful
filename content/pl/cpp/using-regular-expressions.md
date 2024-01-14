---
title:                "C++: Używanie wyrażeń regularnych"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regular expressions (wyrażenia regularne) są kluczowym narzędziem w wielu językach programowania, w tym także w C++. Umożliwiają one wygodne i dokładne wyszukiwanie oraz manipulację tekstem. Dzięki nim możliwe jest szybsze i bardziej precyzyjne podejście do analizy danych tekstowych, co znacznie ułatwia pracę programisty.

## Jak używać

Aby rozpocząć pracę z wyrażeniami regularnymi w C++, musimy najpierw dołączyć bibliotekę <regex> do naszego kodu. Następnie możemy skorzystać z funkcji regex_match, aby sprawdzić czy dany ciąg znaków pasuje do wzorca wyrażenia regularnego, jak pokazano poniżej:

```C++
#include <iostream>
#include <regex>

int main()
{
    std::string text = "Hello world!";
    std::regex pattern("Hello [a-z]+!");
    
    if (std::regex_match(text, pattern))
    {
        std::cout << "Wyrażenie regularne pasuje do tekstu" << std::endl;
    }
    
    return 0;
}
```

Wyjście powyższego kodu to:

```
Wyrażenie regularne pasuje do tekstu
```

Możemy także wykorzystać funkcję regex_search, aby znaleźć wszystkie wystąpienia wzorca w danym ciągu znaków, jak pokazano poniżej:

```C++
#include <iostream>
#include <regex>

int main()
{
    std::string text = "Programowanie jest super fajne!";
    std::regex pattern("super [a-z]+!");
    
    std::smatch match;
    
    while (std::regex_search(text, match, pattern))
    {
        std::cout << "Znalezione dopasowanie: " << match.str(0) << std::endl;
        
        text = match.suffix().str();
    }
    
    return 0;
}
```

Wyjście powyższego kodu to:

```
Znalezione dopasowanie: super fajne!
```

## Głębszy zanurzenie

Poza podstawowym wykorzystaniem, wyrażenia regularne w C++ oferują wiele innych możliwości. Możemy na przykład wykorzystać funkcję regex_replace do dokonania zmian w danym tekście lub regex_iterator do iteracji po wszystkich dopasowaniach. Mamy również dostęp do zaawansowanych konstrukcji, takich jak grupowanie i wyrażenia zwrotne.

Regular expressions w C++ oferują także wiele opcji i flag, które pozwalają na jeszcze większą kontrolę i elastyczność. Jest to szczególnie przydatne w przypadku bardziej skomplikowanych wzorców.

## Zobacz także

- Oficjalna dokumentacja C++ o wyrażeniach regularnych: https://en.cppreference.com/w/cpp/regex
- Przewodnik po wyrażeniach regularnych w C++: https://www.regular-expressions.info/cpp.html
- Szybki przegląd podstaw wyrażeń regularnych w C++: https://www.gormanalysis.com/blog/reading-and-writing-cpp-regular-expressions/