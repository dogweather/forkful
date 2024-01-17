---
title:                "Робота з json"
html_title:           "Java: Робота з json"
simple_title:         "Робота з json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/working-with-json.md"
---

{{< edit_this_page >}}

## Що та чому?
JSON (або JavaScript Object Notation) - це формат для збереження та передачі даних. Його можна легко читати та розуміти як люди, так і комп'ютери. Програмісти використовують JSON, щоб обмінюватися даними між різними системами.

## Як це зробити:
JSON надто простий для програмістів Java, щоб пропустити. Щоб створити JSON, ви можете використовувати об'єкти ```JSONObject``` та ```JSONArray```, або ж скористатися бібліотекою ```org.json```. Нижче наведено приклади коду та відповідні результати:

```
JSONObject student = new JSONObject();
student.put("name", "John Doe");
student.put("age", 25);
student.put("major", "Computer Science");
student.put("grades", new double[]{4.0, 3.9, 3.8});

System.out.println(student.toString());

// Виведе: {"name":"John Doe","age":25,"major":"Computer Science","grades":[4.0,3.9,3.8]}

JSONArray countries = new JSONArray();
countries.put("Ukraine");
countries.put("France");
countries.put("Japan");
countries.put("USA");

System.out.println(countries.toString());

// Виведе: ["Ukraine","France","Japan","USA"]
```

## Основні моменти:
JSON був створений Дугласом Крокфордом у 2001 році та з тих пір став популярним форматом обміну даними. Існують також інші формати, такі як XML та CSV, але JSON має більш просту структуру та займає менше місця. Це робить його ідеальним для використання в мережевих додатках та між системами.

## Дивіться також:
Для отримання додаткової інформації та прикладів використання JSON у Java, перегляньте документацію [Oracle](https://www.oracle.com/technetwork/articles/java/json-1973242.html) та [бібліотеку org.json](https://github.com/stleary/JSON-java).