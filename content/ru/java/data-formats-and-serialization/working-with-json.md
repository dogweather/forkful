---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:18.017851-07:00
description: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON (JavaScript Object\
  \ Notation) \u043F\u043E\u0434\u0440\u0430\u0437\u0443\u043C\u0435\u0432\u0430\u0435\
  \u0442 \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0443 \u044D\u0442\u043E\
  \u0433\u043E \u043B\u0435\u0433\u043A\u043E\u0432\u0435\u0441\u043D\u043E\u0433\u043E\
  \ \u0444\u043E\u0440\u043C\u0430\u0442\u0430 \u043E\u0431\u043C\u0435\u043D\u0430\
  \ \u0434\u0430\u043D\u043D\u044B\u043C\u0438 \u0432 \u0432\u0430\u0448\u0438\u0445\
  \ \u043F\u0440\u0438\u043B\u043E\u0436\u0435\u043D\u0438\u044F\u0445 \u043D\u0430\
  \ Java. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\
  \u2026"
lastmod: '2024-03-13T22:44:44.862434-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON (JavaScript Object Notation)\
  \ \u043F\u043E\u0434\u0440\u0430\u0437\u0443\u043C\u0435\u0432\u0430\u0435\u0442\
  \ \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0443 \u044D\u0442\u043E\u0433\
  \u043E \u043B\u0435\u0433\u043A\u043E\u0432\u0435\u0441\u043D\u043E\u0433\u043E\
  \ \u0444\u043E\u0440\u043C\u0430\u0442\u0430 \u043E\u0431\u043C\u0435\u043D\u0430\
  \ \u0434\u0430\u043D\u043D\u044B\u043C\u0438 \u0432 \u0432\u0430\u0448\u0438\u0445\
  \ \u043F\u0440\u0438\u043B\u043E\u0436\u0435\u043D\u0438\u044F\u0445 \u043D\u0430\
  \ Java."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

## Как:
Давайте подкатим рукава и приступим к кодированию с использованием JSON на Java.

Во-первых, вам понадобится библиотека для обработки JSON, например `Jackson` или `Google Gson`. Здесь мы будем использовать `Jackson`, поэтому добавьте эту зависимость в ваш `pom.xml`:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

Теперь давайте сериализуем (запишем) простой объект Java в JSON:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = new Person("Алекс", 30);
            String json = mapper.writeValueAsString(person);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Person {
    public String name;
    public int age;

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
}
```

Вывод должен быть:

```json
{"name":"Алекс","age":30}
```

Теперь, чтобы десериализовать (прочитать) JSON обратно в объект Java:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Алекс\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + " " + person.age + " лет.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

На выходе будет:

```
Алекс 30 лет.
```

## Глубокое погружение
Простота и эффективность JSON сделали его де-факто стандартом обмена данными в Интернете, сместив XML с его престола. Введенный в начале 2000-х, JSON был произведен из JavaScript, но теперь поддерживается в большинстве языков.

Альтернативы JSON включают XML, который более многословен, и бинарные форматы, такие как Protocol Buffers или MessagePack, которые менее читаемы для человека, но более эффективны по размеру и скорости. Каждый из них имеет свои кейсы использования; выбор зависит от ваших конкретных потребностей в данных и контекста.

В Java, наряду с `Jackson` и `Gson`, у нас есть `JsonB` и `org.json` - другие библиотеки для работы с JSON. Jackson предлагает обработку на основе потоков и известен своей скоростью, в то время как Gson ценится за простоту использования. JsonB является частью Jakarta EE, предлагая более стандартизированный подход.

При реализации работы с JSON, помните о правильной обработке исключений - ваш код должен быть устойчивым к некорректным вводам. Также учитывайте безопасность автоматического связывания данных – всегда валидируйте ваши входные данные!

## См. также
- [Проект Jackson](https://github.com/FasterXML/jackson)
- [Проект Gson](https://github.com/google/gson)
- [Спецификация JSON](https://www.json.org/json-en.html)
- [Спецификация JsonB](https://jakarta.ee/specifications/jsonb/)
