---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "C++: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Co to jest konwersja ciągu znaków na małe litery i dlaczego programiści tego potrzebują?

Konwersja ciągu znaków na małe litery jest procesem zmiany wszystkich liter w ciągu na ich niskie odpowiedniki. Jest to przydatne w programowaniu, ponieważ pozwala na porównywanie i przetwarzanie ciągów znaków w sposób bardziej jednolity. Na przykład, jeśli mamy ciąg "PrzyKłAdoWy CiĄg", to po konwersji na małe litery otrzymamy "przykładowy ciąg", co umożliwi łatwiejsze porównywanie lub przetwarzanie.

Jak to zrobić:

```C++
#include <iostream>
#include <string>
#include <locale>

using namespace std;

int main()
{
    string s = "PrzyKłAdoWy CiĄg";

    //konwersja ciągu na małe litery
    std::locale loc;
    for (char& c : s)
        c = std::tolower(c,loc);

    cout << s; //wyświetli "przykładowy ciąg"

    return 0;
}
```

Pogłębione spojrzenie:

Konwersja ciągu na małe litery jest przydatna w programowaniu, ponieważ pozwala na jednolite traktowanie i porównywanie ciągów znaków niezależnie od tego, czy zawierają one litery wielkie czy małe. Jest to szczególnie ważne przy porównywaniu czy sprawdzaniu równości ciągów, ponieważ zestawienie "ABC" z "abc" może dać nieprawidłowy wynik, jeśli nie są one w tej samej wielkości liter.

Alternatywnym sposobem na konwersję ciągu na małe litery jest użycie funkcji transform() z biblioteki <algorithm>. Można także użyć funkcji toupper() do konwersji na wielkie litery.

Implementacja konwersji na małe litery jest zależna od sposobu obsługi znaków przez system operacyjny, ponieważ niektóre języki mają różne znaki diakrytyczne, które mogą zostać źle przetłumaczone przez prosty algorytm zamiany liter na ich niskie odpowiedniki.

Zobacz także:

- Przykładowy kod konwersji ciągu na małe litery: https://www.includehelp.com/cpp-programs/c-string-to-lower-case.aspx
- Dokumentacja funkcji tolower() z biblioteki <cctype>: http://www.cplusplus.com/reference/cctype/tolower/