---
title:    "C++: Wyciąganie podciągów"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

Wyodrębnianie podciągów jest powszechną operacją w programowaniu, która pozwala na wyodrębnienie określonej części tekstu lub ciągu z danego łańcucha znaków. Jest to bardzo przydatne narzędzie w wielu dziedzinach informatyki, takich jak przetwarzanie tekstu, analiza danych czy tworzenie aplikacji internetowych.

## Jak to zrobić

Aby wyodrębnić podciąg za pomocą języka C++, należy użyć wbudowanej funkcji `substr()`. Przykładowy kod wykorzystujący tę funkcję wyglądałby następująco:

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Przykładowy tekst";
    std::string substr = str.substr(11, 5);

    std::cout << substr; // wynik: tekst
}
```

W powyższym kodzie wyodrębniamy podciąg z łańcucha `str` od indeksu 11 (licząc od zera) i o długości 5 znaków. W rezultacie otrzymujemy podciąg "tekst". Możemy również wyodrębniać podciągi z określoną długością kończące się na danym indeksie, np. `str.substr(0, 7)` zwróci "Przykła".

## Dogłębna analiza

Funkcja `substr()` jest dostępna w bibliotece standardowej języka C++ i służy do wyodrębniania podciągów z łańcucha znaków. Jej sygnatura wygląda następująco: `string substr (size_t pos, size_t len)`, gdzie `pos` to indeks, od którego ma się zacząć wyodrębnianie, a `len` to długość wyodrębnionego podciągu.

Jedną z zalet funkcji `substr()` jest to, że nie zmienia oryginalnego łańcucha, tylko zwraca nowy obiekt typu `string`, zawierający wyodrębniony fragment. Dzięki temu mamy pewność, że oryginalne dane nie zostaną utracone.

Należy także pamiętać, że indeksy w łańcuchach znaków są numerowane od zera, więc pierwszy znak ma indeks 0, drugi 1, itd. Dlatego też jeśli chcemy wyodrębnić pierwszy znak, powinniśmy użyć indeksu 0, a nie 1.

## Zobacz także
- [Dokumentacja funkcji `substr()`](https://www.cplusplus.com/reference/string/string/substr/)
- [Poradnik o wyodrębnianiu podciągów w C++](https://www.geeksforgeeks.org/substring-in-cpp/)
- [Przykłady użycia funkcji `substr()`](https://www.programiz.com/cpp-programming/library-function/string/substr)