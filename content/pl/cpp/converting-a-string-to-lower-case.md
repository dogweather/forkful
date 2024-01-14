---
title:                "C++: Konwertowanie ciągu znaków na małe litery"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Dlaczego

Co sprawia, że konwersja ciągu znaków na małe litery jest ważnym elementem w programowaniu? Wyjaśnijmy.

# Jak to wykonać

Znajdźmy najlepszy sposób na przeprowadzenie konwersji ciągu znaków na mniejsze litery w języku C++. Poniżej znajdują się przykłady kodu i spodziewane wyniki.

```C++
#include <iostream>
#include <string>
#include <algorithm>
using namespace std;

string convertToLower(string input)
{
    // iterujemy przez każdy znak w ciągu znaków
    for (int i = 0; i < input.length(); i++)
    {
        // jeśli znak jest dużą literą, konwertujemy go na małą literę
        if (isupper(input[i]))
        {
            input[i] = tolower(input[i]);
        }
    }
    return input;
}

int main()
{
    // przykładowy ciąg znaków
    string myString = "PROGRAMOWANIE";

    // wywołujemy funkcję convertToLower i przypisujemy wynik do zmiennej
    string result = convertToLower(myString);

    // wyświetlamy wynik
    cout << "Wynik: " << result << endl;

    return 0;
}

```

**Oczekiwany wynik:** "programowanie"

# Głębsze wskazówki

Często konwersja ciągu znaków na mniejsze litery jest potrzebna przy porównywaniu ciągów, aby uniknąć problemów z rozmiarem liter. Istnieje również wiele różnych sposobów na przeprowadzenie tej operacji, zależnie od potrzeb i preferencji programisty. Przykładowo, można użyć funkcji `transform` z biblioteki `<algorithm>` lub użyć metody `tolower` z biblioteki `<cctype>`.

Należy również pamiętać, że konwersja na mniejsze litery może w różny sposób wpływać na różne języki lub systemy operacyjne. Dobrze jest przetestować działanie kodu na różnych platformach, aby mieć pewność, że program działa poprawnie.

# Zobacz również

- [Dokumentacja funkcji isupper w C++](https://www.cplusplus.com/reference/cctype/isupper/)
- [Porównywanie ciągów znaków w C++](https://www.geeksforgeeks.org/stdlexicographical_compare-in-cpp/)