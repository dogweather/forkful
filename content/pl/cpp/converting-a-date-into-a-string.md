---
title:                "Konwersja daty na ciąg znaków"
html_title:           "C++: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Co & Dlaczego?

Konwersja daty na ciąg znaków jest procesem przekształcania daty w formacie liczbowym na tekstowy. Programiści często wykonują tę operację w celu wyświetlenia daty w przyjaznym dla użytkownika formacie.

# Jak to zrobić?

C++ udostępnia wiele metod konwersji daty na ciąg znaków. Przykłady kodu i wyników znajdują się poniżej.

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // Pobranie bieżącej daty
    time_t now = time(0);

    // Konwersja daty na ciąg znaków w formacie "mm/dd/rrrr" 
    char* date = ctime(&now);
    cout << "Bieżąca data: " << date << endl;

    // Konwersja daty na ciąg znaków w formacie "miesiąc dd, rrrr" 
    tm *ltm = localtime(&now);
    cout << "Bieżący miesiąc: " << 1 + ltm->tm_mon << " " << ltm->tm_mday << ", " << 1900 + ltm->tm_year << endl;

    return 0;
}
```
**Wynik:**

```
Bieżąca data: Wed Jul 28 12:25:38 2021
Bieżący miesiąc: 7 28, 2021
```

# Głębszy Zanurzenie

Konwersja daty na ciąg znaków jest powszechna w programowaniu i często wykorzystywana w kontekście wyświetlania dat w formie czytelnej dla użytkownika. Alternatywnym sposobem konwersji daty jest użycie biblioteki Boost, która dostarcza więcej funkcji i może być bardziej szczegółowa w niektórych przypadkach.

Implementacja konwersji daty wymaga wykorzystania funkcji dostępnych w standardowej bibliotece C++ oraz określenia formatu wyjściowego. W przypadku braku odpowiedniego formatu, funkcje mogą zwrócić nieporządane wyniki.

# Zobacz również

[Przewodnik dla programistów C++ - Konwersja daty](https://www.cplusplus.com/reference/ctime/strftime/)