---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:04.188057-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441 YAML\
  \ \u0432 Kotlin \u043E\u0431\u044B\u0447\u043D\u043E \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u0443\u0435\u0442\u0441\u044F \u0431\u0438\u0431\u043B\u0438\u043E\u0442\
  \u0435\u043A\u0430, \u0442\u0430\u043A\u0430\u044F \u043A\u0430\u043A `snakeyaml`.\
  \ \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0440\u0430\u0441\u0441\u043C\u043E\
  \u0442\u0440\u0438\u043C, \u043A\u0430\u043A \u0440\u0430\u0437\u043E\u0431\u0440\
  \u0430\u0442\u044C YAML-\u0444\u0430\u0439\u043B: \u0421\u043D\u0430\u0447\u0430\
  \u043B\u0430\u2026"
lastmod: '2024-03-13T22:44:45.016591-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441 YAML \u0432\
  \ Kotlin \u043E\u0431\u044B\u0447\u043D\u043E \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u0443\u0435\u0442\u0441\u044F \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\
  \u043A\u0430, \u0442\u0430\u043A\u0430\u044F \u043A\u0430\u043A `snakeyaml`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

## Как это сделать:
Для работы с YAML в Kotlin обычно используется библиотека, такая как `snakeyaml`. Давайте рассмотрим, как разобрать YAML-файл:

Сначала добавьте зависимость в файл `build.gradle`:

```kotlin
implementation("org.yaml:snakeyaml:1.29")
```

Теперь давайте разберем простой YAML-файл с использованием SnakeYAML:

```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.InputStream

fun main() {
    val yaml = Yaml()
    val inputStream: InputStream = this::class.java.classLoader.getResourceAsStream("config.yaml")
    val data: Map<String, Any> = yaml.load(inputStream)

    println(data["name"])
    println(data["age"])
}

// Пример содержимого config.yaml:
// name: John Doe
// age: 30

// Пример вывода:
// John Doe
// 30
```

Этот фрагмент кода загружает YAML-файл и печатает значения, связанные с ключами `name` и `age`.

## Глубокое погружение
YAML появился в начале 2000-х годов, чтобы бороться со сложностью XML. Он предлагает более простой синтаксис, делая его предпочтительным для файлов конфигурации. Альтернативы включают в себя JSON, который более ориентирован на данные и менее удобен для чтения человеком, а также TOML, который занимает некоторую среднюю позицию. При работе с YAML в Kotlin библиотеки, такие как `snakeyaml`, обеспечивают механизм разбора, интегрируясь с вашим кодом Kotlin для преобразования строк YAML в нативные структуры данных.

## Смотрите также
- Спецификация YAML 1.2: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- Документация по Kotlin: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
