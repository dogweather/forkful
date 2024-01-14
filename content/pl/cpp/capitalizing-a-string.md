---
title:                "C++: Zastosowanie wielkich liter do ciągu znaków"
simple_title:         "Zastosowanie wielkich liter do ciągu znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

C++ jest niezwykle popularnym językiem programowania, który jest wykorzystywany do tworzenia różnych aplikacji i programów. Jednym z często używanych zadań jest manipulacja łańcuchami tekstowymi. W tym poradniku dowiedz się, dlaczego warto umieć kapitalizować łańcuchy tekstowe w C++ i jak to zrobić.

## Jak To Zrobić

Kapitalizowanie łańcucha tekstowego to proces zamiany wszystkich liter na duże. W C++, istnieje kilka sposobów na to, w zależności od tego jak duża kontrola nad łańcuchem tekstowym chcemy mieć.

### 1. Użycie biblioteki <string>

Pierwszym sposobem jest użycie gotowej biblioteki <string>, która dostarcza nam funkcję `toupper()` do zamiany pojedynczej litery na dużą. Możemy wykorzystać pętlę for, aby przejść przez cały łańcuch i zamienić każdą literę.

```C++
#include <iostream>
#include <string>

int main() {

  std::string text = "to jest przykładowy łańcuch tekstowy";
  int size = text.size();

  for (int i = 0; i < size; i++) {
    text[i] = toupper(text[i]);
  }

  std::cout << text << std::endl;

  return 0;
}
```
**Output:** TO JEST PRZYKŁADOWY ŁAŃCUCH TEKSTOWY

### 2. Użycie funkcji <algorithm>

Drugim sposobem jest użycie funkcji `transform()` z biblioteki <algorithm>. Ta funkcja przyjmuje dwa parametry: iterator początkowy i końcowy oraz funkcję, która będzie apliczowana na każdym elemencie. W naszym przypadku, wykorzystamy funkcję `toupper` jako trzeci parametr.

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {

  std::string text = "to jest przykładowy łańcuch tekstowy";

  transform(text.begin(), text.end(), text.begin(), toupper);

  std::cout << text << std::endl;

  return 0;
}
```
**Output:** TO JEST PRZYKŁADOWY ŁAŃCUCH TEKSTOWY

### 3. Użycie funkcji toupper

Ostatni sposób nie wymaga importowania żadnych bibliotek. Możemy wykorzystać funkcję `toupper` bezpośrednio na naszym łańcuchu tekstowym.

```C++
#include <iostream>
#include <string>

int main() {

  std::string text = "to jest przykładowy łańcuch tekstowy";
  int size = text.size();

  for (int i = 0; i < size; i++) {
    text[i] = toupper(text[i]);
  }

  std::cout << text << std::endl;

  return 0;
}
```
**Output:** TO JEST PRZYKŁADOWY ŁAŃCUCH TEKSTOWY

## Deep Dive

Warto zauważyć, że w C++ mamy także funkcje `tolower()` i `toupper()` do zmiany liter na małe lub duże, co może być przydatne w niektórych aplikacjach. Ponadto, warto zwrócić uwagę na to, że funkcje `toupper()` i `tolower()` przyjmują jako argumenty wartości typu int, co pozwala nam wykorzystać je do manipulacji kodami ASCII.

## Zobacz także

- [Dokumentacja <string>](https://en.cppreference.com/w/cpp/header/string)
- [Dokumentacja <algorithm>](https://en.cppreference.com/w/cpp/header/algorithm)
- [Funkcje toupper i tolower](https://en.cppreference.com/w/cpp/string/byte/toupper)

Dziękujemy za przeczytanie tego poradnika. Mamy nadzieję, że teraz wiesz, dlaczego kapitalizowanie łańcuchów tekstowych jest ważne w