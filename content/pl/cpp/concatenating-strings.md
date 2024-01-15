---
title:                "Łączenie ciągów znaków"
html_title:           "C++: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszym artykule omówimy temat łączenia (ang. concatenation) ciągów tekstowych w języku programowania C++. Dowiecie się, dlaczego jest to ważne oraz jak tego dokonać.

## Jak To Zrobić

Łączenie ciągów tekstowych to operacja, która pozwala na połączenie dwóch lub więcej ciągów w jeden duży ciąg. W C++ możemy tego dokonać na kilka sposobów, wykorzystując różne funkcje i znaki.

### Wykorzystanie operatora +

Jednym ze sposobów na łączenie ciągów w C++ jest wykorzystanie operatora +. Przykładowy kod wyglądałby następująco:

```C++
#include <iostream>

using namespace std;

int main() {
    string firstName = "John";
    string lastName = "Doe";
    string fullName = firstName + lastName;

    cout << "Witaj " << fullName << "!" << endl;
    return 0;
}
```

W tym przypadku, wynikiem działania programu będzie wyświetlenie na ekranie tekstu "Witaj John Doe!".

### Użycie metody append()

Innym sposobem na łączenie ciągów tekstowych jest wykorzystanie metody append(), która dodaje określony ciąg na końcu innego. Przykładowy kod prezentuje się następująco:

```C++
#include <iostream>

using namespace std;

int main() {
    string hello = "Cześć ";
    string name = "Ania";
    hello.append(name);

    cout << hello << endl;
    return 0;
}
```

Wynikiem działania programu będzie wyświetlenie tekstu "Cześć Ania" na ekranie.

### Wykorzystanie funkcji strcat()

Kolejną możliwością jest użycie funkcji strcat(), która dokonuje konkatenacji ciągów i zapisuje je w pierwszym podanym argumencie. Przykładowy kod wyglądałby tak:

```C++
#include <iostream>
#include <cstring>

using namespace std;

int main() {
    char sentence[50] = "Ciąg dalszy ciągu ";
    char addition[10] = "tekstowego!";
    strcat(sentence, addition);

    cout << sentence << endl;
    return 0;
}
```

W wyniku działania programu na ekranie zostanie wyświetlony tekst "Ciąg dalszy ciągu tekstowego!".

## Deep Dive

Podczas dokonywania łączenia ciągów tekstowych w C++, istnieje kilka rzeczy, których należy być świadomym:

- Typ zwracany przez operację + to obiekt typu string, a nie ciąg znaków (ang. character array).
- Operator += jest skróconą wersją operatora +, co oznacza, że wyrażenie "a += b" jest równoważne "a = a + b".
- W funkcji strcat() pierwszym argumentem musi być wskaźnik na ciąg znaków (lub tablicę znaków), natomiast drugim argumentem może być zarówno wskaźnik jak i tablica.

## Zobacz też

- [C++ Podstawy: Ciągi znaków](https://www.edureka.co/blog/strings-in-cpp/)
- [Operacje na ciągach tekstowych](https://www.programiz.com/cpp-programming/string)
- [Dokumentacja C++: Funkcja strcat()](https://www.cplusplus.com/reference/cstring/strcat/)