---
title:                "Java: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## Для чого

YAML є одним із найпопулярніших форматів для зберігання та передачі даних, особливо в програмуванні. Це є дуже зручним форматом, оскільки його можна легко читати як люди, так і комп'ютери. Це також підтримується більшістю мов програмування, в тому числі і Java.

Тому, якщо ви займаєтеся програмуванням і хочете використовувати добре структурований формат для своїх даних, робота з YAML буде необхідною навичкою для вас.

## Як

Для початку, нам потрібно додати залежність для бібліотеки SnakeYAML у наш проект. Це можна зробити в файлі pom.xml, додавши наступну залежність:

```
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.27</version>
</dependency>
```

Після цього, ми можемо почати працювати з YAML у нашому коді:

```
import org.yaml.snakeyaml.Yaml;

String yamlString = "name: John\nage: 25";

Yaml yaml = new Yaml();
Map<String, Object> data = yaml.load(yamlString);

System.out.println(data.get("name")); // John
System.out.println(data.get("age")); // 25
```

Тут ми спочатку визначаємо стандартний рядок YAML, а потім зберігаємо його в об'єкт Yaml. Завдяки цьому, ми можемо легко витягнути дані, використовуючи ключі.

Також, ми можемо зберегти дані з Java об'єкту у форматі YAML:

```
Map<String, Object> data = new HashMap<>();
data.put("name", "John");
data.put("age", 25);

Yaml yaml = new Yaml();
String yamlString = yaml.dump(data);

System.out.println(yamlString); // "name: John\nage: 25"
```

## Поглиблення

Окрім основних прикладів використання, YAML також підтримує різні типи даних, включаючи рядки, числа, логічні значення та списки. Крім того, ви можете використовувати коментарі та вкладені структури даних, щоб зробити ваші файли YAML більш зрозумілими.

Також варто знати, що ви можете використовувати більш складні методи загрузки та збереження даних з використанням класу Yaml і його параметрів. Детальніше про це можна прочитати в [документації SnakeYAML](https://bitbucket.org/asomov/snakeyaml/src/default/README.md).

## Дивіться також

- [SnakeYAML documentation](https://bitbucket.org/asomov/snakeyaml/src/default/README.md)
- [Офіційний сайт YAML](https://yaml.org/)
- [Стаття про YAML від Java programmer](https://www.baeldung.com/java-snake-yaml)