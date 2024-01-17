---
title:                "Робота з yaml"
html_title:           "Kotlin: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

Що і чому?

Робота з YAML - це одна з найбільш популярних форматів для представлення даних. Цей формат використовується в багатьох програмах та мовах програмування, і має на меті створення читабельних та зручних для редагування файлів конфігурації та обміну інформацією між різними програмними продуктами. Програмісти працюють з YAML, щоб спростити процес роботи з даними та забезпечити легку читабельність.

Як це зробити:

```Kotlin
val yamlString = """
  recipe: 
    - name: Pizza
      ingredients:
        - dough
        - cheese
        - tomatoes
        - pepperoni
    - name: Tacos
      ingredients:
        - tortillas
        - chicken
        - avocado
"""
val yamlObject = Yaml().load(yamlString)
// Вивід:
// {recipe=[{name=Pizza, ingredients=[dough, cheese, tomatoes, pepperoni]}, {name=Tacos, ingredients=[tortillas, chicken, avocado]}]}
```

Залишаючись на поверхні:

Історичний контекст:
YAML був вперше створений у 2001 році і був прийнятий програмістами як альтернатива формату XML. Він швидко набув популярності завдяки своїй простоті та здатності до зберігання структурованих даних.

Альтернативи:
Основною альтернативою YAML є формат JSON, який також використовується для представлення даних. Однак, YAML має більш дружелюбний синтаксис та може зберігати більш складні структури даних.

Деталі реалізації:
Кодування та декодування YAML в Kotlin можна виконувати за допомогою бібліотеки jackson-yaml, яка надає зручний інтерфейс для роботи з YAML.

Див. також:
- Офіційна документація YAML (https://yaml.org/spec/1.2/spec.html)
- Офіційна документація Kotlin (https://kotlinlang.org/docs/home.html)