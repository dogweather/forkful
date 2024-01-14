---
title:                "C: Praca z JSON"
simple_title:         "Praca z JSON"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

Po co zabierać się za pracę z JSON w języku C? JSON (JavaScript Object Notation) to popularny format danych, szczególnie w przypadku komunikacji między serwerem a klientem. W tym blog postu omówimy w jaki sposób możesz pracować z tym formatem w języku C.

## Jak to zrobić

Aby pracować z JSON w języku C, musisz najpierw zaimportować bibliotekę *json-c*, która dostarcza narzędzia do manipulacji danymi JSON. Poniżej znajduje się przykładowy kod przedstawiający jak stworzyć obiekt JSON i wypisać go na ekranie:

```C
#include <stdio.h>
#include <json-c/json.h>

int main() {
    // tworzymy obiekt JSON
    struct json_object *myobj = json_object_new_object();
    // dodajemy do niego pola i wartości
    json_object_object_add(myobj, "imie", json_object_new_string("Jan"));
    json_object_object_add(myobj, "wiek", json_object_new_int(25));
    // wypisujemy na ekranie
    printf("%s\n", json_object_to_json_string(myobj));
    // usuwamy obiekt z pamięci
    json_object_put(myobj);
    return 0;
}
```

Powyższy kod wyprodukowałby następujący output:

```json
{"imie":"Jan","wiek":25}
```

Możesz również parsować dane JSON przychodzące z serwera za pomocą funkcji *json_tokener_parse()*. Poniższy kod pokazuje jak wydobyć dane z obiektu JSON i przypisać je do zmiennych:

```C
#include <stdio.h>
#include <json-c/json.h>

int main() {
    // przykładowy string z danymi JSON
    char *json_string = "{\"imie\": \"Anna\", \"wiek\": 30}";
    // parsujemy dane
    struct json_object *myobj = json_tokener_parse(json_string);
    // wyciągamy imie i wiek
    struct json_object *name;
    struct json_object *age;
    json_object_object_get_ex(myobj, "imie", &name);
    json_object_object_get_ex(myobj, "wiek", &age);
    // przypisujemy wartości do zmiennych
    const char *imie = json_object_get_string(name);
    int wiek = json_object_get_int(age);
    // wypisujemy na ekran
    printf("Imię: %s, Wiek: %d\n", imie, wiek);
    // usuwamy obiekt z pamięci
    json_object_put(myobj);
    return 0;
}
```

Powyższy kod wyprodukowałby następujący output:

```text
Imię: Anna, Wiek: 30
```

## Głębsze zagadnienia

Ponadto, biblioteka *json-c* oferuje wiele innych funkcji, takich jak modyfikowanie obiektów JSON, tworzenie tablic i wiele innych. Warto przejrzeć dokumentację aby poznać wszystkie możliwości tej biblioteki.

## Zobacz również

- Dokumentacja biblioteki *json-c*: https://github.com/json-c/json-c
- Przykłady kodów wykorzystujących *json-c*: https://github.com/json-c/json-c/tree/master/tests
- Poradniki wideo o pracy z JSON w języku C: https://www.youtube.com/playlist?list=PL2UmzTIzxgLh-9T7hWkmVjn9Zlxm0rsU5