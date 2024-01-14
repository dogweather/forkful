---
title:                "C++: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu, może istnieć potrzeba konwertowania ciągu znaków na małe litery. Może to być pomocne przy porównywaniu ciągów lub przy wyświetlaniu danych w jednolitym stylu. W tym blogu dowiesz się, jak łatwo wykonać tę konwersję w języku C++.

## Jak to zrobić

W C++ konwersja ciągu znaków na małe litery jest bardzo prosta. Wystarczy użyć funkcji `tolower()` na każdym znaku w ciągu i przypisać go do nowego ciągu. Poniżej znajduje się przykładowy kod z wykorzystaniem pętli `for`:

```C++
#include <iostream>
#include <string>

using namespace std;

int main()
{
    string str = "HELLO WORLD";
    string new_str = "";

    for (int i = 0; i < str.length(); i++)
    {
        new_str += tolower(str[i]);
    }

    cout << new_str; // output: hello world

    return 0;
}
```

Możemy również wykorzystać pętlę `for-each` lub funkcję `transform()` w bibliotece `<algorithm>`, aby uprościć ten proces. Poniżej znajdują się przykłady z wykorzystaniem tych metod:

```C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main()
{
    string str = "HELLO WORLD";

    // przy użyciu pętli for-each
    for (char &c: str)
    {
        c = tolower(c);
    }
    cout << str; // output: hello world

    // przy użyciu funkcji transform()
    transform(str.begin(), str.end(), str.begin(), ::tolower);
    cout << str; // output: hello world

    return 0;
}
```

## Wnikliwe zanurzenie

W języku C++ istnieją różne sposoby na konwersję ciągu znaków na małe litery, w tym także wykorzystanie biblioteki `<locale>`. Jednak funkcja `tolower()` jest najprostszym i najczęściej wykorzystywanym sposobem. Warto również zauważyć, że w przypadku języków z alfabetem niestandardowym (np. japońskim), należy użyć innych metod dostępnych w bibliotece `<locale>`.

## Zobacz również

- [Convert String to Lowercase in C++](https://www.geeksforgeeks.org/converting-strings-multiple-languages-supported-c/)
- [Locale in C++](https://www.programiz.com/cpp-programming/library-function/cctype/tolower)