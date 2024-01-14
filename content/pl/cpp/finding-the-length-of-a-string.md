---
title:    "C++: Znajdowanie długości ciągu znaków"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

Często w procesie programowania musimy operować na ciągach znaków, a jedną z podstawowych informacji o nich jest ich długość. Dlatego znajomość sposobu znajdowania długości napisów jest ważną umiejętnością dla każdego programisty.

## Jak to zrobić

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string napis = "Witaj świecie!";
    // Najprostszy sposób: użycie funkcji wbudowanej length()
    cout << "Długość napisu: " << napis.length() << endl;

    // Możemy także użyć pętli for do przejrzenia wszystkich znaków i zliczenia ich ilości
    int dlugosc = 0;
    for (char& c : napis)
        dlugosc++;
    cout << "Długość napisu: " << dlugosc << endl;

    // Inna metoda to użycie iteratora do przejścia przez napis i zliczenia znaków
    int licznik = 0;
    for (string::iterator it = napis.begin(); it != napis.end(); ++it)
        licznik++;
    cout << "Długość napisu: " << licznik << endl;

    // Wreszcie, możemy też użyć funkcji wbudowanej strlen() z biblioteki cstring
    // Wymaga ona jednak przekonwertowania napisu do formatu char*
    char napis2[] = "Witaj świecie!";
    cout << "Długość napisu: " << strlen(napis2) << endl;

    return 0;
}
```

**Output:**
```
Długość napisu: 15
Długość napisu: 15
Długość napisu: 15
Długość napisu: 15
```

**Uwaga:** Należy pamiętać, że funkcja wbudowana length() jest dostępna tylko w przypadku typu string, dlatego w przypadku użycia innych typów danych, jak na przykład char* lub int, musimy korzystać z innych metod.

## Bardziej wnikliwe spojrzenie

Długość napisu jest tak naprawdę ilością pamięci zajmowanej przez poszczególne znaki w danym napisie. Dlatego też, w przypadku użycia funkcji length() dla typu string, program najpierw musi zliczyć wszystkie znaki w nim zawarte, a następnie zwrócić ich ilość. W przypadku użycia pętli for lub iteratora, program wykonuje to samo zadanie, ale musi przejrzeć całego napis i zliczyć wszystkie znaki. Dlatego też, metody te mogą być nieco wolniejsze w porównaniu do funkcji length().

## Zobacz także

- [C++ String Length - GeeksforGeeks](https://www.geeksforgeeks.org/length-of-string-in-cpp/)
- [C++ Standard Library: Strings](https://www.learncpp.com/cpp-tutorial/char-arrays-and-c-style-strings/)
- [C++ strchr, strlen, strcpy, strcmp functions](https://www.programiz.com/cpp-programming/library-function/cstring/strchr)