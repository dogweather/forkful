---
title:    "C++: Konwertowanie ciągu znaków na małe litery"
keywords: ["C++"]
---

{{< edit_this_page >}}

# Dlaczego konwertowanie ciągu znaków na małe litery jest przydatne?

Konwertowanie ciągu znaków na małe litery jest przydatne, ponieważ pomaga w ujednoliceniu danych i ułatwia porównywanie tekstu. Ponadto, wiele funkcji i algorytmów wymaga danych w małych literach, więc konwertowanie może usprawnić pracę programisty.

## Jak to zrobić

```C++
// Przykładowy kod w C++ konwertujący ciąg znaków na małe litery
#include <iostream>
#include <cstdlib>
#include <string>
#include <algorithm>
using namespace std;

// Funkcja konwertująca ciąg znaków na małe litery
string toLowerCase(string str) {
    // iterujemy po każdym znaku w ciągu i konwertujemy go na małą literę
    // używając funkcji `tolower` z biblioteki `algorithm`
    for_each(str.begin(), str.end(), [](unsigned char &c) {
        c = tolower(c);
    });
    return str;
}

int main() {
    // przykładowy ciąg znaków
    string myString = "ABCDE";
    // wyświetlamy oryginalny ciąg
    cout << "Oryginalny ciąg: " << myString << endl;
    // wywołujemy funkcję do konwertowania na małe litery
    myString = toLowerCase(myString);
    // wyświetlamy zkonwertowany ciąg
    cout << "Ciąg po konwersji: " << myString << endl;
    return 0;
}
```

Przykładowy output:

```
Oryginalny ciąg: ABCDE
Ciąg po konwersji: abcde
```

## Wnikliwiej o konwertowaniu ciągu znaków na małe litery

Konwertowanie ciągu znaków na małe litery może się wydawać prostym zadaniem, ale wymaga pewnej wiedzy dotyczącej typów danych oraz działania funkcji `tolower` z biblioteki `algorithm`. Ważne jest również pamiętanie o uwzględnieniu języka używanego w projekcie, ponieważ niektóre litery mogą być różnie konwertowane w zależności od alfabetu.

## Zobacz również

- [Dokumentacja funkcji `tolower` z biblioteki `algorithm` w języku C++](https://www.cplusplus.com/reference/cctype/tolower/)
- [Porównywanie tekstu w języku C++](https://www.geeksforgeeks.org/application-compare-two-strings/)
- [Właściwości i operacje na ciągach znaków w języku C++](https://www.tutorialspoint.com/cpp/cpp_strings.htm)