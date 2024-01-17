---
title:                "Praca z formatem json"
html_title:           "Kotlin: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Co & dlaczego?

Programowanie z użyciem JSON (Javascript Object Notation) polega na tworzeniu i manipulowaniu danymi w formacie tekstowym, wykorzystując składnię języka JavaScript. Programiści często korzystają z JSON, ponieważ jest to prosty i popularny sposób na wymianę danych między aplikacjami na różnych platformach.

## Jak to zrobić:

Utworzenie obiektu JSON:

```Kotlin
var json = "{"name": "Anna", "age": 25}"
```

Dodanie nowego klucza i wartości do obiektu:

```Kotlin
json.put("hobby", "painting")
```

Konwersja obiektu JSON na string:

```Kotlin
var jsonAsString = json.toString()
```

## Głębsza analiza:

JSON został stworzony w 2001 roku przez Douglasa Crockforda jako lżejsza alternatywa dla formatu XML. JSON jest często wykorzystywany do przesyłania danych w aplikacjach internetowych, mobilnych oraz IoT (Internet of Things). Można go również wykorzystać do zapisywania danych lokalnie w plikach.

Alternatywnymi sposobami przechowywania danych są na przykład format CSV lub XML. Więcej informacji na ten temat można znaleźć na stronie: https://www.json.org/json-pl.html.

## Zobacz także:

- Oficjalna dokumentacja języka Kotlin dotycząca pracy z JSON: https://kotlinlang.org/docs/reference/using-gradle.html
- Narzędzia do pracy z JSON w środowisku programistycznym IntelliJ IDEA: https://plugins.jetbrains.com/plugin/7294-kotlin-json
- Przykładowe projekty z wykorzystaniem JSON na platformie GitHub: https://github.com/search?q=kotlin+json