---
title:                "Kotlin: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

# Чому

Кожен розробник, який працює з веб-додатками, незважаючи на мову програмування, часто зіштовхується з необхідністю працювати з конфігураційними файлами. І одним з найпоширеніших варіантів є формат YAML. Цей язик ідентичний зі стандартним посиланням на об'єкти даних в реальному світі, зробивши його важливим для розробників, які не хочуть використовувати важкорозбірний JSON.

# Як

Зараз давайте подивимся на приклад Kotlin, який демонструє, як просто працювати з YAML.

```Kotlin
val yamlString = """
                   |- name: Bob
                   |  age: 25
                   |  occupation: developer
                   |- name: Alice
                   |  age: 30
                   |  occupation: designer
                   """.trimMargin()

val yamlList = Yaml().load(yamlString) as List<Map<String, Any>>

for (person in yamlList) {
    println("${person["name"]} - ${person["occupation"]}")
}
```

Результат виконання цього коду буде виглядати так:

```
Bob - developer
Alice - designer
```

# Глибокий погляд

Окрім простого читання і записування даних в YAML, ця мова також має багато інших корисних можливостей, таких як наступні:

- Вбудована підтримка для коментарів, включаючи багаторядкові коментарі.
- Можливість використання змінних та інших типів даних для створення більш складних структур даних.
- Легкість і простота редагування YAML файлів без необхідності використання спеціальних програм.
- Структурованість даних, що робить роботу з ними більш зручною та підвищує читабельність файлів.
- Підтримка багатьох мов програмування, включаючи Kotlin, Python, Java та інші.

# Дивись також

- [Офіційна документація YAML](https://yaml.org/)
- [Резюме по YAML: 5 причин чому його варто використовувати в своїх проектах](https://www.honeybadger.io/blog/a-beginner-s-guide-to-yaml/)
- [Як працювати з конфігураційними файлами в Kotlin за допомогою бібліотеки SnakeYAML](https://www.baeldung.com/snakeyaml-kotlin)