---
title:                "Korzystanie z wyrażeń regularnych"
html_title:           "Arduino: Korzystanie z wyrażeń regularnych"
simple_title:         "Korzystanie z wyrażeń regularnych"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Regularne wyrażenia to sekwencje znaków, które służą do dopasowywania i manipulowania łańcuchów. Programiści używają ich przede wszystkim do przeszukiwania, dzielenia i zastępowania łańcuchów, co może znacznie ułatwić pracę.

## Jak to zrobić:
Oto bardzo prosty przykład użycia regularnych wyrażeń w C++:

```C++
#include <regex>
#include <string>
#include <iostream>

int main() {
    std::string text = "Całkiem przyjemny; dzień, nieprawdaż?";
    std::regex delim("[;,]");
    std::sregex_token_iterator iter(text.begin(), text.end(), delim, -1);
    std::sregex_token_iterator end;
    for ( ; iter != end; ++iter)
        std::cout << *iter << '\n';
    return 0;
}
```
W wyniku otrzymamy:

```
Całkiem przyjemny
 dzień
 nieprawdaż?
```
Jak widać, nasz program rozdzielił napis na podstawie średników i przecinków.

## Deep Dive
Regularne wyrażenia wywodzą się z teorii języków formalnych i automatów, do której to teorii powstały w latach '50. W C++, regularne wyrażenia zostały wprowadzone w standardzie C++11. Mimo że są bardzo wygodne, nie zawsze są one idealnym rozwiązaniem - dla prostych zadań często łatwiej i szybciej jest użyć standardowych funkcji na łańcuchy. Co więcej, implementacja regularnych wyrażeń w bibliotece standardowej C++ jest dość skomplikowana i może wpływać na wydajność programu, więc warto z nich korzystać rozważnie.

## Zobacz również
Jeśli chcesz dowiedzieć się więcej o regularnych wyrażeniach w C++, sprawdź te źródła:

1. Dokumentacja C++: [link](http://www.cplusplus.com/reference/regex/)
3. StackOverflow: "C++ Regular expressions in practice": [link](https://stackoverflow.com/questions/37854322/c-regular-expressions-in-practice)