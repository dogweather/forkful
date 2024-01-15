---
title:                "Робота з json"
html_title:           "C++: Робота з json"
simple_title:         "Робота з json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

Цей пост допоможе вам зрозуміти, чому подальша робота з JSON може бути корисною та важливою для вас як програміста. JSON є одним з найпопулярніших форматів обміну даними в сучасному програмуванні і використовується в багатьох веб-додатках та сервісах.

## Як то зробити

Найпопулярнішим способом роботи з JSON у C++ є використання бібліотеки RapidJSON. Почніть з встановлення цієї бібліотеки у ваш проект. Далі заповнюйте об'єкти даними, використовуючи методи бібліотеки. Цей код демонструє таку роботу:

```C++
#include <iostream>
#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"

using namespace rapidjson;

int main() {
  // встановлення документу
  Document document;
  document.SetObject();

  // додавання даних у об'єкт
  Document::AllocatorType& allocator = document.GetAllocator();
  document.AddMember("name", "John", allocator);
  document.AddMember("age", 30, allocator);
  document.AddMember("isEmployed", true, allocator);

  // перетворення у JSON-рядок
  StringBuffer buffer;
  Writer<StringBuffer> writer(buffer);
  document.Accept(writer);

  // виведення результату
  std::cout << buffer.GetString() << std::endl;

  return 0;
}
```

Результатом цього коду буде наступне:

```json
{"name": "John", "age": 30, "isEmployed": true}
```

Якщо ви вже маєте готовий JSON-об'єкт, а потрібні лише дані з нього, то використовуйте метод `Parse()`:

```C++
#include <iostream>
#include "rapidjson/document.h"

using namespace rapidjson;

int main() {
  // приклад рядка з JSON
  const char* json = "{\"name\": \"John\", \"age\": 30, \"isEmployed\": true}";

  // парсінг рядка
  Document document;
  document.Parse(json);

  // виведення даних
  std::cout << "Name: " << document["name"].GetString() << std::endl;
  std::cout << "Age: " << document["age"].GetInt() << std::endl;

  if (document["isEmployed"].GetBool()) {
    std::cout << "Employed: Yes" << std::endl;
  } else {
    std::cout << "Employed: No" << std::endl;
  }

  return 0;
}
```

Цей шматок коду виведе наступне:

```
Name: John
Age: 30
Employed: Yes
```

## Глибокий пошук

Розуміння роботи з JSON потребує знання його структури. У JSON можна використовувати різні типи даних, такі як рядки, числа, булеві значення, масиви та об'єкти. Також важливо знати правила форматування та роботи з бібліотекою RapidJSON для ефективного взаємодії з даними. Рекомендуємо ознайомитись з офіційною документацією бібліотеки та практикуватись з ї