---
title:                "C++: Робота з json"
simple_title:         "Робота з json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

Завжди корисно вміти працювати з JSON в сучасному програмуванні. Цей формат данних широко використовується для обміну інформацією між різними серверами та додатками. Вміння працювати з JSON допоможе вам створювати більш динамічні та зручні програми.

## Як

```C++
#include <iostream>
#include <nlohmann/json.hpp>  // бібліотека для роботи з JSON

using namespace std;
using json = nlohmann::json;

int main(){

    // створюємо новий об'єкт типу JSON
    json data;

    // додаємо дані в об'єкт
    data["name"] = "Max";
    data["age"] = 26;
    data["hobby"] = "programming";

    // конвертуємо об'єкт у строку
    string output = data.dump();

    // виводимо отриманий результат
    cout << output << endl;

    return 0;
}
```

Вихідний результат:

```C++
{"name":"Max","age":26,"hobby":"programming"}
```

## Глибоке дослідження

Хоча формат JSON дуже популярний та зручний для роботи, потрібно звернути увагу на кілька особливостей, щоб уникнути помилок. Наприклад, важливо дотримуватися правильної структури та синтаксису при створенні та обробці об'єктів JSON. Також важливо враховувати, що формат не підтримує деякі типи даних, наприклад, нульовий рядок чи деякі символи. Для більш детального дослідження рекомендується ознайомитися з офіційною документацією та практикувати на різних прикладах.

## Дивіться також

- [Офіційна документація з JSON](https://www.json.org/json-en.html)
- [Бібліотека для роботи з JSON у C++](https://github.com/nlohmann/json)
- [Детальніше про синтаксис та особливості формату JSON](https://www.w3schools.com/js/js_json_syntax.asp)