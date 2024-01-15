---
title:                "Praca z formatem json"
html_title:           "C++: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON (JavaScript Object Notation) jest jednym z najpopularniejszych formatów do przechowywania i przesyłania danych w języku C++. Jest on łatwy do czytania, tworzenia i przetwarzania przez programy, co czyni go idealnym wyborem do pracy z danymi w aplikacjach.

## Jak to zrobić

Aby rozpocząć pracę z JSON w języku C++, musimy najpierw zaimportować bibliotekę "json.hpp". Możemy to zrobić za pomocą dyrektywy `#include`:

```C++
#include "json.hpp"
```

Następnie, aby utworzyć obiekt JSON, możemy skorzystać z konstruktora `json::object()`. Przykładowo, możemy utworzyć obiekt JSON zawierający imię, nazwisko i wiek osoby:

```C++
nlohmann::json person = nlohmann::json::object({
  {"name", "Kasia"},
  {"surname", "Nowak"},
  {"age", 25}
});
```

Możemy również utworzyć tablicę JSON za pomocą konstruktora `json::array()`:

```C++
nlohmann::json fruits = nlohmann::json::array({"apple", "orange", "banana"});
```

Aby dodać nowe wartości do obiektu lub tablicy JSON, możemy użyć metody `push_back()`:

```C++
fruits.push_back("grape");
person.push_back({"address", "ul. Długa 25"});
```

Możemy także pobierać dane z obiektu lub tablicy JSON za pomocą operatora `[]` i przypisać je do zmiennych:

```C++
std::string name = person["name"];
int age = person["age"];
```

Aby zobaczyć pełny przykład działania na obiekcie i tablicy JSON, zapraszamy do zapoznania się z dokumentacją biblioteki [json.hpp](https://github.com/nlohmann/json).

## Deep Dive

Powyższe przykłady przedstawiają podstawowe funkcjonalności związane z pracą z JSON w języku C++. Jednakże, użytkownicy mogą również skorzystać z wielu innych metod dostępnych w bibliotece "json.hpp", takich jak `erase()`, `find()` czy `clear()`, aby jeszcze bardziej dostosować swoje operacje z JSON-em.

Warto również wspomnieć, że biblioteka "json.hpp" umożliwia konwersję danych pomiędzy JSON-em a formatami takimi jak JSON Pointer, JSON Merge Patch czy CBOR (Concise Binary Object Representation). Dzięki temu, możliwe jest jeszcze łatwiejsze włączanie danych JSON w aplikacji.

## Zobacz także

Jeśli jesteś zainteresowany/a nauką więcej o pracy z JSON w języku C++, polecamy zapoznać się z poniższymi źródłami:

- [Oficjalna dokumentacja biblioteki "json.hpp"](https://github.com/nlohmann/json#documentation)
- [Poradnik na temat pracy z JSON-em w języku C++](https://www.plazywaczka.pl/blog/programowanie/c-jak-pracowac-z-jsonem/)
- [Darmowy kurs "JSON w C++" na platformie edukacyjnej Udemy](https://www.udemy.com/course/json-w-c-plus-plus/)