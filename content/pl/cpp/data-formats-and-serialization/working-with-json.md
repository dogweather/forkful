---
title:                "Praca z JSON"
aliases:
- /pl/cpp/working-with-json/
date:                  2024-02-03T19:21:56.795905-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

JSON (JavaScript Object Notation) to lekki format służący do przechowywania i transportowania danych, co czyni go doskonałym medium do wymiany danych między serwerami a aplikacjami internetowymi. Programiści używają JSON ze względu na jego łatwą czytelność dla ludzi i prostotę parsowania przez maszyny, szczególnie podczas pracy nad aplikacjami wymagającymi wymiany danych przez internet lub ustawienia konfiguracji.

## Jak to zrobić:

W C++ nie ma natywnego wsparcia dla JSON, jednak biblioteki stron trzecich, takie jak nlohmann/json, ułatwiają pracę. Oto jak jej użyć do podstawowych zadań:

Najpierw upewnij się, że masz zainstalowaną bibliotekę. Jeśli korzystasz z menedżera pakietów takiego jak vcpkg lub Conan, łatwo możesz dodać `nlohmann/json` do swojego projektu.

### Parsowanie JSON z ciągu znaków

```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // Dane JSON jako ciąg znaków
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // Parsowanie ciągu JSON
    auto jsonObject = nlohmann::json::parse(jsonData);

    // Dostęp do danych
    std::cout << "Name: " << jsonObject["name"] << "\n"
              << "Age: " << jsonObject["age"] << "\n"
              << "City: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**Przykładowe wyjście:**

```
Name: John
Age: 30
City: New York
```

### Generowanie JSON

Tworzenie danych JSON jest równie proste; po prostu przypisujesz wartości do obiektu `nlohmann::json`.

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // Tworzenie obiektu JSON
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // Konwersja obiektu JSON na ciąg znaków i wydruk
    std::string jsonString = jsonObject.dump(4); // Argument 4 służy do ładnego formatowania
    std::cout << jsonString << std::endl;

    return 0;
}
```

**Przykładowe wyjście:**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

Te przykłady demonstrują podstawowe funkcjonalności pracy z JSON w C++ przy użyciu biblioteki `nlohmann/json`. Znając te podstawy, możesz parsować i generować JSON dla różnych zastosowań, od plików konfiguracyjnych po wymianę danych w aplikacjach sieciowych.
