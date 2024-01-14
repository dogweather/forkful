---
title:                "C++: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu jest konieczne usunięcie znaków pasujących do określonego wzorca. To może być część procesu weryfikacji wprowadzonych danych lub po prostu czyszczenie tekstu z niechcianych elementów. W tym blogu pokażemy, jak to zrobić w języku programowania C++.

## Jak to zrobić

Aby usunąć znaki pasujące do danego wzorca, musimy skorzystać z funkcji `std::regex_replace()` z biblioteki `<regex>`. Funkcja ta wymaga trzech argumentów - tekstu, w którym chcemy dokonać zmian, wzorca do znalezienia oraz ciągu znaków, na który chcemy je zmienić.

```C++
#include <iostream>
#include <regex>
using namespace std;

int main() {
   string text = "Hello 123 World!";

   // usuwanie wszystkich cyfr z tekstu
   string pattern = "\\d";
   string replaced = regex_replace(text, regex(pattern), "");

   cout << replaced << endl;
   // wyświetli "Hello World!"

   return 0;
}
```

W powyższym przykładzie, najpierw definiujemy zmienną `text` z tekstem, który chcemy zmodyfikować. Następnie tworzymy zmienną `pattern`, która reprezentuje wzorzec, w tym przypadku `\d`, który oznacza wszystkie cyfry. Wreszcie, wywołujemy funkcję `regex_replace()` z podanymi argumentami i wypisujemy zmodyfikowany tekst na ekranie.

## Deep Dive

Funkcja `std::regex_replace()` jest bardzo przydatna do usuwania znaków pasujących do określonych wzorców, ale warto zauważyć, że jej wykorzystanie może mieć wpływ na wydajność naszego programu. W niektórych przypadkach, lepszą opcją może być użycie funkcji `std::replace_if()` lub `std::remove_if()` z biblioteki `<algorithm>`. 

`std::remove_if()` działa na kontenerach, takich jak `std::string` lub `std::vector`, a jego zadaniem jest przeniesienie wszystkich elementów spełniających podany warunek na koniec kontenera. Następnie, możemy usunąć te elementy z kontenera za pomocą funkcji `erase()`.

```C++
#include <iostream>
#include <algorithm>
using namespace std;

int main() {
   string text = "Hello 123 World!";

   // usuwanie wszystkich cyfr z tekstu
   text.erase(remove_if(text.begin(), text.end(), ::isdigit), text.end());

   cout << text << endl;
   // wyświetli "Hello World!"

   return 0;
}
```

Warto również zwrócić uwagę na wyrażenia regularne używane w funkcji `std::regex_replace()`. Są one bardzo potężnym narzędziem, pozwalającym na precyzyjne wyszukiwanie i modyfikację tekstu. Wymagają jednak pewnego stopnia znajomości, aby móc je skutecznie wykorzystać. 

## Zobacz również

- [Podstawy programowania w C++](https://www.tutorialspoint.com/cplusplus/index.htm)
- [Dokumentacja funkcji `std::regex_replace()`](https://en.cppreference.com/w/cpp/string/regex/replace)