---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:43.440178-07:00
description: "\u042F\u043A: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0437\u0430\
  \u043A\u0430\u0441\u0430\u0454\u043C\u043E \u0440\u0443\u043A\u0430\u0432\u0438\
  \ \u0442\u0430 \u043F\u043E\u0447\u043D\u0435\u043C\u043E \u043A\u043E\u0434\u0443\
  \u0432\u0430\u0442\u0438 \u0437 JSON \u0443 Java. \u041F\u043E-\u043F\u0435\u0440\
  \u0448\u0435, \u0432\u0430\u043C \u0437\u043D\u0430\u0434\u043E\u0431\u0438\u0442\
  \u044C\u0441\u044F \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0430\
  \ \u0434\u043B\u044F \u043E\u0431\u0440\u043E\u0431\u043A\u0438 JSON, \u043D\u0430\
  \u043F\u0440\u0438\u043A\u043B\u0430\u0434, `Jackson` \u0430\u0431\u043E `Google\u2026"
lastmod: '2024-03-13T22:44:49.115815-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0437\u0430\u043A\u0430\u0441\
  \u0430\u0454\u043C\u043E \u0440\u0443\u043A\u0430\u0432\u0438 \u0442\u0430 \u043F\
  \u043E\u0447\u043D\u0435\u043C\u043E \u043A\u043E\u0434\u0443\u0432\u0430\u0442\u0438\
  \ \u0437 JSON \u0443 Java."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

## Як:
Давайте закасаємо рукави та почнемо кодувати з JSON у Java.

По-перше, вам знадобиться бібліотека для обробки JSON, наприклад, `Jackson` або `Google Gson`. Тут ми використовуватимемо `Jackson`, тому додайте цю залежність до вашого `pom.xml`:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

Тепер давайте серіалізуємо (запишемо) простий Java-об'єкт у JSON:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = new Person("Alex", 30);
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

Вивід має бути:

```json
{"name":"Alex","age":30}
```

Тепер, щоб десеріалізувати (прочитати) JSON назад у Java-об'єкт:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Alex\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + " is " + person.age + " років.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Вивід буде:

```
Алекс є 30 років.
```

## Поглиблено
Простота і ефективність JSON зробили його де-факто стандартом для обміну даними в мережі, позбавивши трону XML. Введений на початку 2000-х, JSON був похідним від JavaScript, але зараз підтримується більшістю мов.

Альтернативи JSON включають XML, який є більш многословним, та бінарні формати, як-от Protocol Buffers або MessagePack, які менш зручні для читання людиною, але більш ефективні за розміром і швидкістю. Кожен з них має свої випадки використання; вибір залежить від ваших конкретних потреб і контексту даних.

В Java, крім `Jackson` і `Gson`, ми маємо `JsonB` і `org.json` як інші бібліотеки для обробки JSON. Jackson пропонує обробку на основі потоків і відомий своєю швидкістю, тоді як Gson відзначається легкістю використання. JsonB є частиною Jakarta EE, пропонуючи більш стандартизований підхід.

При реалізації JSON пам'ятайте про належну обробку винятків - ваш код має бути стійким до некоректних вхідних даних. Також зверніть увагу на безпекові імплікації автоматичного зв'язування даних – завжди перевіряйте свої вхідні дані!

## Див. також
- [Проект Jackson](https://github.com/FasterXML/jackson)
- [Проект Gson](https://github.com/google/gson)
- [Специфікація JSON](https://www.json.org/json-en.html)
- [Специфікація JsonB](https://jakarta.ee/specifications/jsonb/)
