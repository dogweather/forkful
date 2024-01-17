---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "C++: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Długość ciągu znaków to liczba znaków wewnątrz danego ciągu. Programiści często muszą znaleźć długość ciągu znaków w swoich programach, ponieważ jest to ważna informacja, która pomaga im zrozumieć i manipulować danymi.

## Jak to zrobić:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string str = "Hello World";
  int len = str.length();
  
  cout << "Długość ciągu znaków: " << len << endl;
  
  return 0;
}
```

Output:

```
Długość ciągu znaków: 11
```

## Głębszy zanurzenie:

1. Kontekst historyczny:
Pierwszym językiem programowania, który wprowadził funkcję do znajdowania długości ciągu znaków, był język programowania BCPL, który został stworzony w latach 60. przez Martina Richardsa. Od tego czasu funkcja ta została wzięta pod uwagę przez inne języki programowania, w tym przez C++, co czyni ją jednym z podstawowych elementów tego języka.

2. Alternatywy:
Oprócz użycia funkcji length(), istnieją również inne sposoby na znalezienie długości ciągu znaków w C++, takie jak użycie pętli for lub wbudowanej funkcji strlen().

3. Szczegóły implementacji:
Funkcja length() w C++ jest zaimplementowana w bibliotece standardowej jako metoda klasy string. Wewnątrz funkcji następuje iteracja przez wszystkie elementy stringa i zliczanie ich liczby. W przypadku pustego stringa, funkcja zwraca wartość 0.

## Zobacz także:

- [Dokumentacja funkcji length() w C++](https://www.cplusplus.com/reference/string/string/length/)
- [Różne sposoby na znajdowanie długości ciągu znaków w C++](https://www.geeksforgeeks.org/length-of-a-string-using-pointer-and-array-both/)
- [Porównanie wydajności funkcji length() i strlen() w C++](https://stackoverflow.com/questions/8710719/what-is-the-difference-between-strlen-and-length-for-stdstring)