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

##Чому
YAML - це простий та зручний формат для збереження структурованих даних. Він широко використовується в різних галузях програмування, таких як веб-розробка, автоматизація, тестування та багато іншого. Розуміння його синтаксису та вміння працювати з ним може значно полегшити роботу з даними у вашому проєкті.

##Як
```Kotlin
// оголошення YAML-документу за допомогою стандартної бібліотеки SnakeYAML
val yaml = Yaml()
// створення об'єкту String для збереження YAML-запису
val yamlString = """
    name: John
    age: 30
    hobbies:
     - coding
     - reading
    """.trimIndent()
// зчитування YAML-документу у вигляді мапи
val dataMap = yaml.load(yamlString) as Map<String, Any>
// виведення даних з мапи
println("Name: ${dataMap["name"]}")
println("Age: ${dataMap["age"]}")
println("Hobbies: ${dataMap["hobbies"]}")
```

Результат:
```
Name: John
Age: 30
Hobbies: [coding, reading]
```

##Занурення
Як ви можете побачити з прикладу вище, синтаксис YAML досить простий та легко зрозуміти. Він базується на відступах для вказівників структури даних, що робить його дружнім для читання та редагування людиною. Крім того, даний формат підтримує коментарі, що дозволяє додавати пояснення до структури даних.

Також, YAML має можливість використання вкладеності для структур даних, що дозволяє зберігати більш складну інформацію в одному файлі. Це особливо корисно для конфігураційних файлів, які містять багато параметрів та можуть потребувати організації в декілька рівнів.

##Дивись також
- [Офіційна документація YAML](https://yaml.org/)
- [Записи про YAML на блозі Snowblossom](https://snowblossom.org/tutorials/tag/yaml/)
- [Розбираємося з YAML у Kotlin](https://dzone.com/articles/the-kotlin-way-of-working-with-yaml)