---
title:                "Konkatenacja ciągów znaków"
html_title:           "Bash: Konkatenacja ciągów znaków"
simple_title:         "Konkatenacja ciągów znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Łączenie łańcuchów to proces łączenia dwóch lub więcej ciągów znaków w jeden dłuższy ciąg. Programiści robią to, aby skutecznie manipulować i organizować dane tekstowe.

## Jak to zrobić:
W C++ jednym ze sposobów na łączenie łańcuchów jest zastosowanie operatora `+`. Zobaczmy jak to działa.

```C++
#include<iostream>
#include<string>

int main() {
    std::string str1 = "Dzień";
    std::string str2 = " dobry";
    std::string str3 = str1 + str2; // concatenate strings
    std::cout << str3 << std::endl; // It will print: Dzień dobry
    return 0;
}
```
## Głębsze spojrzenie:
Łączenie łańcuchów znaków jest już od dawna ważnym narzędziem w programowaniu. W starszych językach, takich jak C, łączenie łańcuchów było bardziej skomplikowane i wymagało korzystania z funkcji takich jak `strcat()`. W C++, dzięki klasie `std::string`, jest to o wiele prostsze.

Alternatywą dla bezpośredniego użycia operatora `+` jest metoda `append()` klasy `std::string`.

```C++
std::string str1 = "Cześć, ";
std::string str2 = "świecie!";
str1.append(str2); // concatenate using append()
std::cout << str1 << std::endl; // It will print: Cześć, świecie!
```
Łączenie łańcuchów znaków C++ jest wygodne, ale nie zawsze jest najefektywniejsze. W przypadku dużej ilości danych tekstowych, lepiej jest używać funkcji takich jak `std::stringstream`.

## Zobacz też:
Aby dowiedzieć się więcej na temat łączenia łańcuchów znaków w C++, zobacz te dodatkowe źródła:
* Dokumentacja C++ na stronie [cplusplus.com](http://www.cplusplus.com/reference/string/string/operator+/)
* Wyniki na temat łączenia łańcuchów znaków na [Stack Overflow](https://stackoverflow.com/questions/144130/how-do-i-concatenate-a-stdstring-and-an-int)
* Szczegółowe informacje na temat `std::stringstream` na [cplusplus.com](http://www.cplusplus.com/reference/sstream/stringstream/)