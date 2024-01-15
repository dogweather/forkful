---
title:                "Wydobywanie podciągów"
html_title:           "C++: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego?

Jeśli często pracujesz z ciągami znaków w swoim kodzie C++, prawdopodobnie musiałeś już wyodrębniać podciągi wewnątrz nich. W tym artykule dowiesz się, dlaczego warto robić to w odpowiedni sposób i jak można to zrobić w praktyce.

## Jak to zrobić?

Aby wyodrębnić podciąg ze stringa, możesz użyć metody `substr()` razem z indeksami początkowym i końcowym, określającymi zakres, z którego ma zostać wycięty podciąg. Na przykład:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Deklaracja stringa wejściowego
    string input = "Cześć świecie!";
    
    // Wyodrębnienie podciągu od indeksu 6 do końca
    string output1 = input.substr(6);
    // Wyodrębnienie podciągu od indeksu 6 do 4 dalej
    string output2 = input.substr(6, 4);
    
    // Wyświetlenie wyników
    cout << output1 << endl; // "świecie!"
    cout << output2 << endl; // "świe"
    
    return 0;
}
```

W powyższym przykładzie wykorzystaliśmy metody `substr()` wraz z indeksami, aby wyodrębnić odpowiednie podciągi ze stringa. Warto pamiętać, że indeksy numerowane są od zera, a drugi parametr, określający długość wyodrębnionego podciągu, jest opcjonalny.

## Deep Dive

Metoda `substr()` może nie tylko pomagać w wyodrębnianiu podciągów, ale również w łączeniu ich. Jeśli jako drugi parametr przekażesz długość wyodrębnionego podciągu, możesz ustalić także maksymalną długość wyjściowego stringa. Na przykład:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Deklaracja dwóch stringów
    string input1 = "Witaj";
    string input2 = "świecie";
    
    // Wykonanie konkatenacji z wykorzystaniem substr()
    string output = input1 + input2.substr(1, 3);
    
    // Wyświetlenie wyniku
    cout << output << endl; // "Witawie"
    
    return 0;
}
```

Powyższy przykład pokazuje, że możemy wykorzystać metodę `substr()` w celu włączenia wyodrębnionego podciągu w innym miejscu wewnątrz stringa. Jest to dużo wygodniejsze niż ręcznie manipulowanie indeksami stringów.

## Zobacz również

- [Dokumentacja metody substr()](https://en.cppreference.com/w/cpp/string/basic_string/substr)
- [Porównanie wydajności różnych metod wyodrębniania podciągów w C++](https://stackoverflow.com/questions/51712191/c-how-is-stdstring-substr-implemented)
- [Przydatne porady i triki związane z stringami w C++](https://www.geeksforgeeks.org/tricks-c-string/)