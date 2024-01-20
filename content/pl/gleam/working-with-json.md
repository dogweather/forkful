---
title:                "Praca z formatem json"
html_title:           "Gleam: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

JSON jest to popularny format do przechowywania i przesyłania danych w postaci tekstu. Programiści często używają go do przechowywania danych w swoich aplikacjach, ponieważ jest prosty w użyciu i wszechstronny.

## Jak?

Aby przetwarzać dane JSON w Gleam, musimy najpierw zaimportować bibliotekę standardową [gleam/json](https://github.com/gleam-lang/gleam/blob/master/lib/json/src/json.gleam). Następnie możemy użyć jednej z funkcji tej biblioteki, takich jak `decode` lub `encode`, aby odpowiednio zamieniać dane z formatu JSON na format Gleam lub z formatu Gleam na format JSON.

```Gleam
import json

pub struct Person(name: String, age: Int)

let json_data = """
  {
    "name": "John",
    "age": 28
  }
"""

let person = json.decode(json_data, Person)
// person = Person("John", 28)

let person_json = json.encode(person)
// person_json = "{\"name\":\"John\",\"age\":28}"
```

## Wiecej

JSON pojawił się po raz pierwszy w 2001 roku i od tego czasu jest jednym z najpopularniejszych formatów do przetwarzania danych. Alternatywami są na przykład formaty XML i YAML, ale JSON jest uważany za bardziej przejrzysty i czytelny. Implementacja biblioteki [gleam/json](https://github.com/gleam-lang/gleam/blob/master/lib/json/src/json.gleam) jest oparta o specyfikację [RFC 7159](https://tools.ietf.org/html/rfc7159).

## Zobacz także

* [Oficjalna dokumentacja Gleam](https://gleam.run/)
* [Proste wyjaśnienie czym jest JSON](https://www.json.org/json-pl.html)