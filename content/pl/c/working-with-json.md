---
title:                "Praca z formatem json"
html_title:           "C: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/working-with-json.md"
---

{{< edit_this_page >}}

##Czym jest i dlaczego programiści tego używają? 

JSON (JavaScript Object Notation) jest formatem danych, który jest powszechnie używany przez programistów do przesyłania informacji między aplikacjami internetowymi. Jest to format przypominający język JavaScript, więc jest bardzo czytelny dla programistów. Programiści korzystają z JSON, ponieważ jest to prosty i wygodny sposób przesyłania danych.

##Jak to zrobić:

Aby pracować z JSON w C, musimy korzystać z biblioteki json-c, która dostarcza narzędzia dla programistów do przetwarzania danych JSON. Oto przykładowy kod:

```C
#include <stdio.h>
#include "json-c/json.h"

int main() {
  // tworzenie obiektu JSON
  json_object *obj = json_object_new_object();
  
  // dodawanie kluczy i wartości do obiektu
  json_object_object_add(obj, "name", json_object_new_string("Jan Kowalski"));
  json_object_object_add(obj, "age", json_object_new_int(30));
  
  // konwertowanie obiektu do formatu tekstowego
  const char *json_string = json_object_to_json_string(obj);
  
  // wypisanie na ekranie
  printf("Obiekt JSON: %s\n", json_string);
  
  // zwalnianie pamięci
  json_object_put(obj);
  
  return 0;
}
```

Przykładowy wynik:

```
Obiekt JSON: {"name": "Jan Kowalski", "age": 30}
```

##Głębsze wgląd:

JSON został stworzony w 2001 roku i od tego czasu stał się jednym z najczęściej używanych formatów danych w świecie programowania. Alternatywami dla JSON są między innymi XML i CSV, jednak JSON jest bardziej czytelny i prostszy do przetwarzania.

W bibliotece json-c można znaleźć również funkcje pozwalające na parsowanie i odczytywanie plików JSON z dysku. W celu uzyskania dokładnej dokumentacji, można odwiedzić stronę projektu na GitHubie.

##Zobacz też:

- Strona projektu json-c na GitHubie: https://github.com/json-c/json-c
- Oficjalna strona formatu JSON: https://www.json.org/