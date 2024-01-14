---
title:                "Java: Робота з json"
simple_title:         "Робота з json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

JSON - це один з найпоширеніших форматів даних, які використовуються для обміну інформацією між різними системами. Робота з JSON є важливою в галузі програмування, оскільки це дозволяє ефективно обробляти та передавати дані.

## Як працювати з JSON у Java

Найпростіший спосіб створити об'єкт JSON в Java - використовуючи бібліотеку Gson:

```Java
import com.google.gson.Gson;
import com.google.gson.JsonObject;

// створюємо об'єкт Gson
Gson gson = new Gson();

// створюємо об'єкт JsonObject
JsonObject json = new JsonObject();

// додаємо дані у об'єкт
json.addProperty("name", "John");
json.addProperty("age", 25);

// перетворюємо у рядок
String jsonString = gson.toJson(json);

// виводимо результат
System.out.println(jsonString);
```

**Вихід:** `{"name":"John","age":25}`

Для роботи з JSON-файлами у Java, можна використовувати клас `JsonReader` з бібліотеки Gson:

```Java
import com.google.gson.Gson;
import com.google.gson.stream.JsonReader;

// створюємо об'єкт Gson
Gson gson = new Gson();

// читаємо з файлу
JsonReader reader = new JsonReader(new FileReader("users.json"));

// створюємо масив користувачів
User[] users = gson.fromJson(reader, User[].class);

// виводимо результат
System.out.println(users[0].getName() + " " + users[0].getAge());
```

**Вихід:** `John 25`

## Глибокий занурення у роботу з JSON

JSON має багато корисних можливостей, наприклад, управління об'єктами засобами Javascript, ефективну обробку даних через вкладені об'єкти та масиви, зручне читання та ін. Крім того, можна використовувати анотації для зручного мапінгу даних з JSON-файлу на об'єкти у Java.

Для докладнішої інформації щодо використання Gson та роботи з JSON у Java, рекомендую ознайомитися з [офіційною документацією](https://sites.google.com/site/gson/gson-user-guide) та [цією статтею](https://medium.com/@javatech/json-serialization-deserialization-in-java-using-google-gson-be08256c8447).

## Дивись також

Ознайомитися з іншими корисними інструментами для роботи з JSON у Java можна за посиланнями нижче:

- [Jackson](https://github.com/FasterXML/jackson)
- [JSON Simple](https://github.com/fangyidong/json-simple)
- [JSON-io](https://github.com/jdereg/json-io)