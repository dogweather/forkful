---
title:                "C++: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jedną z podstawowych operacji w programowaniu jest łączenie ze sobą ciągów znaków, czyli tzw. concatenacja. Jest to nieodłączny element wielu algorytmów i aplikacji, a także przydatny sposób na tworzenie czytelnych informacji dla użytkownika. W tym artykule dowiesz się, dlaczego warto nauczyć się łączyć ze sobą ciągi znaków w języku C++.

## Jak to zrobić

W języku C++, do łączenia ciągów znaków używa się operatora plus (+). W poniższym przykładzie pokazane jest, jak utworzyć nowy ciąg znaków, łącząc ze sobą dwa już istniejące:

 ```C++
#include <iostream>
using namespace std;

int main() {
    string name = "Jan";
    string lastName = "Kowalski";
    string fullName = name + " " + lastName;
    cout << fullName << endl; // wypisze: Jan Kowalski
    return 0;
}
```

Możemy także łączyć więcej niż dwa ciągi znaków, wykorzystując pętlę for i funkcję append():

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string words[] = {"hej", "to", "jest", "zestaw", "słów"};
    string sentence = "";
    for (string word : words) {
        sentence.append(word + " ");
    }
    cout << sentence << endl; // wypisze: hej to jest zestaw słów
    return 0;
}
```

## Głębszy wgląd

Język C++ oferuje także wiele innych metod do łączenia ciągów znaków, takich jak insert() czy replace(). Warto również pamiętać o dokładnym kontroli długości łączonego ciągu, aby uniknąć problemów z pamięcią.

Podczas pracy z ciągami znaków, ważne jest także zastosowanie odpowiedniego formatowania dla czytelności. W tym celu można użyć funkcji setw() lub setfill() dostępnych w bibliotece <iomanip>.

## Zobacz także

- [Dokumentacja C++ o konwersji i łączeniu ciągów znaków](https://en.cppreference.com/w/cpp/string/basic_string)
- [Tutorial o łączeniu ciągów znaków w C++](https://www.udemy.com/course/cpp-programming-for-beginners/)
- [Przydatne techniki formatowania ciągów znaków w C++](https://www.learncpp.com/cpp-tutorial/54-formatted-console-input-output/)

Dzięki umiejętności łączenia ciągów znaków w języku C++, możesz tworzyć czytelne i praktyczne aplikacje. Pamiętaj o wykorzystaniu odpowiednich metod i formatowania, aby uniknąć błędów i ułatwić sobie pracę z ciągami znaków. Życzę owocnej nauki!