---
title:                "Работа с YAML"
date:                  2024-01-29T00:05:04.188057-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

YAML, что расшифровывается как "YAML Ain't Markup Language" (YAML - это не язык разметки), является стандартом сериализации данных, предназначенным для чтения человеком. Программисты используют его для конфигурации программного обеспечения, определения данных или установки параметров благодаря его универсальности и удобству чтения.

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
