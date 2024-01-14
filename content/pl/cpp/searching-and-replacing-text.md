---
title:                "C++: Wyszukiwanie i zastępowanie tekstu."
simple_title:         "Wyszukiwanie i zastępowanie tekstu."
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub początkującym w nauce języka C++, na pewno spotkasz się z potrzebą wyszukiwania i zamiany tekstu w swoim kodzie. Jest to nieodłączna część procesu tworzenia oprogramowania - czasami trzeba zmienić nazwę funkcji lub poprawić błędy ortograficzne, a czasochłonne byłoby wykonywanie tego ręcznie. Dlatego warto poznać narzędzie, które ułatwi nam to zadanie.

## Jak to zrobić?

Aby przeprowadzić wyszukiwanie i zamianę tekstu w C++, musimy skorzystać z funkcji "find" i "replace" biblioteki string. Pierwszą rzeczą, jaką musimy zrobić, jest wczytanie naszego tekstu do zmiennej typu string. Możemy to zrobić w następujący sposób:

```C++

#include <iostream>
#include <string>
using namespace std;

int main(){

string text = "To jest przykładowy tekst do zmiany.";

// dalsza część kodu

return 0;
}
```

Następnie, w celu wyszukania fragmentu tekstu, używamy funkcji "find", podając jako argumenty poszukiwaną frazę oraz pozycję, od której chcemy rozpocząć wyszukiwanie. Na przykład, jeśli chcemy zmienić wszystkie wystąpienia słowa "tekst" na "kod", możemy to zrobić w ten sposób:

```C++

#include <iostream>
#include <string>
using namespace std;

int main(){

string text = "To jest przykładowy tekst do zmiany.";

int position = text.find("tekst", 0);
while (position != string::npos){
    text.replace(position, 5, "kod");
    position = text.find("tekst", position + 1);
}

// wyświetlamy zmieniony tekst
cout << text;

return 0;
}
```

W powyższym przykładzie, korzystając z pętli, przeszukujemy cały tekst i zamieniamy każde znalezione słowo "tekst" na "kod". Po wykonaniu tego kodu, nasza zmienna "text" będzie przechowywać wartość "To jest przykładowy kod do zmiany." 

## Głębsza analiza

Język C++ oferuje wiele innych funkcji do wyszukiwania i zamiany tekstu, takich jak "find_first_of" czy "replace_copy". Ponadto, możemy również użyć wyrażeń regularnych, aby jeszcze dokładniej określić szukane frazy. Znajomość tych różnych metod i możliwości pozwoli nam jeszcze skuteczniej manipulować tekstem w naszym programie.

## Zobacz także

- [Dokumentacja funkcji "find" w C++](https://www.cplusplus.com/reference/string/string/find/)
- [Tutorial o wyrażeniach regularnych w C++](https://www.educative.io/edpresso/what-are-regular-expressions-in-cpp)