---
title:                "Робота з JSON"
aliases:
- /uk/java/working-with-json/
date:                  2024-02-03T19:23:43.440178-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?
Робота з JSON (JavaScript Object Notation) означає обробку цього легковісного формату обміну даними у ваших Java-додатках. Програмісти вибирають JSON для серіалізації та передачі структурованих даних через мережу, а також для легкої конфігурації і зберігання даних, оскільки він є зрозумілим для людини та незалежним від мови.

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
