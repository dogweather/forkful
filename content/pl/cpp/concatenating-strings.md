---
title:                "C++: Łączenie ciągów znaków"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Manipulacja łańcuchami znaków jest nieodłączną częścią programowania w C++. Często zachodzi potrzeba połączenia kilku wyrazów lub zmiennych w jeden dłuższy wyraz. W tym artykule dowiesz się, dlaczego i jak używać funkcji concatenate, aby ułatwić sobie pracę.

## Jak to zrobić

Do łączenia stringów w C++ używamy operatora "+" lub funkcji ```std::string::append()```. Operator "+" pozwala na bezpośrednie połączenie dwóch zmiennych typu string.

Przykład:

```C++
#include <iostream>
#include <string>

int main() {
    std::string first_name = "Karolina";
    std::string last_name = "Nowak";
    std::string full_name = first_name + " " + last_name;

    std::cout << "Moje imię to: " << full_name << std::endl;
    return 0;
}

//Output: Moje imię to: Karolina Nowak
```

Funkcja ```std::string::append()``` jest wykorzystywana, gdy chcemy do istniejącego łańcucha znaków dodać kolejny wyraz lub zmienną.

Przykład:

```C++
#include <iostream>
#include <string>

int main() {
    std::string sentence = "Lubię ";
    std::string fruit = "banany";
    sentence.append(fruit);

    std::cout << "Wydrukuj to zdanie: " << sentence << "!" << std::endl;
    return 0;
}

//Output: Wydrukuj to zdanie: Lubię banany!
```

## Głębokie zgłębianie

W przypadku łączenia większej ilości zmiennych lub dłuższych wyrazów, może pojawić się pytanie, które z metod jest szybsze i wydajniejsze. W praktyce, nie ma dużych różnic w wydajności pomiędzy użyciem operatora "+" i funkcji ```std::string::append()```, więc wybór zależy od preferencji programisty.

Warto jednak zwrócić uwagę na wydajność przy łączeniu dużych ilości wyrazów w pętli. W takim przypadku lepiej jest użyć funkcji ```std::stringstream```, która umożliwia skuteczne łączenie wielu zmiennych typu string w jeden łańcuch.

Przykład:

```C++
#include <iostream>
#include <sstream>

int main() {
    std::stringstream sentence;
    int num_of_apples = 3;
    std::string fruit = "jabłka";

    for (int i = 1; i <= num_of_apples; ++i) {
        sentence << i << " " << fruit << " ";
    }

    std::cout << "Potrzebujesz kupić: " << sentence.str() << std::endl;
    return 0;
}

//Output: Potrzebujesz kupić: 1 jabłka 2 jabłka 3 jabłka
```

## Zobacz również

1. [Tutorial C++ na W3Schools](https://www.w3schools.com/cpp/cpp_strings.asp)
2. [Podstawy łańcuchów znaków w C++](https://re-codex.it/2018/11/working-with-strings-in-cpp-pt-1-the-very-basics/)
3. [Dokumentacja C++ dla funkcji std::string::append](https://en.cppreference.com/w/cpp/string/basic_string/append)