---
title:                "Робота з json"
html_title:           "C#: Робота з json"
simple_title:         "Робота з json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Що і для чого?:
Робота з JSON - це метод обробки даних в програмуванні, який дозволяє зберігати та передавати дані у вигляді об'єктів у форматі тексту. Програмісти використовують цей формат для обміну даними між різними системами та зберігання структурованих даних з різних джерел.

## Як це зробити:
```C#
// Приклад створення JSON об'єкту:
var person = new { name = "John", age = 30, city = "Kyiv" };

// Приклад кодування об'єкту в JSON формат:
var json = JsonConvert.SerializeObject(person);
Console.WriteLine(json);

// Виведення результату:
// {"name":"John","age":30,"city":"Kyiv"}
```

## Підробиці:
JSON (JavaScript Object Notation) був розроблений в 1999 році і став популярним використовувати як спосіб передачі даних. Цей формат є альтернативою XML та є більш спрощеним та зручним для опрацювання. У програмуванні, дані у форматі JSON можна легко перетворювати в об'єкти та здійснювати обробку даних. Для роботи з JSON у C# існує багато бібліотек, таких як Newtonsoft.Json, які спрощують роботу з цим форматом.

## Дивіться також:
- [JSON у Вікіпедії](https://uk.wikipedia.org/wiki/JSON)
- [Офіційна документація по Newtonsoft.Json](https://www.newtonsoft.com/json/help/html/Introduction.htm)