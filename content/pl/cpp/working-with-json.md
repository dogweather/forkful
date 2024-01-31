---
title:                "Praca z JSON"
date:                  2024-01-19
simple_title:         "Praca z JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
JSON, czyli JavaScript Object Notation, to format wymiany danych. Programiści używają go, bo jest lekki, łatwy do czytania i pisanie w nim jest łatwe zarówno dla ludzi, jak i maszyn.

## Jak to zrobić:
W C++ do pracy z JSON-em potrzebujemy zewnętrznej biblioteki, np. `nlohmann/json`. Spójrzmy na przykład:

```C++
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // Tworzenie JSON-a w C++
    nlohmann::json jsonExample;
    jsonExample["nazwa"] = "Jan Kowalski";
    jsonExample["wiek"] = 30;
    jsonExample["hobby"] = {"książki", "gry", "muzyka"};

    // Wyświetlanie JSON-a
    std::cout << jsonExample.dump(4) << std::endl;

    // Parsowanie JSON-a z ciągu znaków
    std::string jsonString = R"({"miasto":"Warszawa","ulica":"Marszałkowska"})";
    nlohmann::json parsedJson = nlohmann::json::parse(jsonString);

    // Wyświetlanie sparsowanego JSON-a
    std::cout << parsedJson.dump(4) << std::endl;
}
```

Przykładowe wyjście:
```
{
    "nazwa": "Jan Kowalski",
    "wiek": 30,
    "hobby": [
        "książki",
        "gry",
        "muzyka"
    ]
}
{
    "miasto": "Warszawa",
    "ulica": "Marszałkowska"
}
```

## Deep Dive:
JSON jest w użyciu od początku 2000 roku. Alternatywą dla JSON-a jest XML, ale JSON zyskał na popularności dzięki swej prostocie. C++ nie ma wbudowanego wsparcia dla JSON-a, dlatego korzystamy z bibliotek takich jak `nlohmann/json` czy `jsoncpp`. `nlohmann/json` jest zalecaną opcją ze względu na prostotę użycia i czytelność kodu, który za jej pomocą piszemy.

## Zobacz również:
- Oficjalne repozytorium `nlohmann/json`: https://github.com/nlohmann/json
- Dokumentacja `jsoncpp`: https://github.com/open-source-parsers/jsoncpp
- Wprowadzenie do JSON-a: https://www.json.org/json-pl.html
