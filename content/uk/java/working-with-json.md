---
title:                "Робота з JSON"
date:                  2024-01-19
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (Що і чому?)
JSON обмінює дані. Простий, швидкий, легко читається людьми та машинами. Прогери люблять: гнучкість і стандартизація.

## How to: (Як це зробити:)
```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        ObjectMapper mapper = new ObjectMapper();
        String jsonInput = "{\"name\":\"Yaroslav\",\"age\":30}";

        try {
            // Deserialize JSON to Java object
            Person person = mapper.readValue(jsonInput, Person.class);
            System.out.println("Name: " + person.getName() + ", Age: " + person.getAge());

            // Serialize Java object to JSON
            String jsonOutput = mapper.writeValueAsString(person);
            System.out.println(jsonOutput);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    static class Person {
        private String name;
        private int age;

        // Getters and setters...
    }
}
```
Sample Output:
```
Name: Yaroslav, Age: 30
{"name":"Yaroslav","age":30}
```

## Deep Dive (Поглиблене занурення)
JSON виник у 2000-х, швидко замінив XML. Альтернативи: YAML, BSON. Імплементація: бібліотеки Jackson, Gson, org.json. Серіалізація: від об'єкта до рядка JSON. Десеріалізація: навпаки.

## See Also (Дивись також)
- [JSON в Java з Jackson](https://www.baeldung.com/jackson)
- [Офіційний сайт JSON](https://www.json.org/json-en.html)
- [Сравнение JSON бібліотек для Java](https://www.baeldung.com/java-json)
