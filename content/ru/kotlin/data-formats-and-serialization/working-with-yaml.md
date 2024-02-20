---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:04.188057-07:00
description: "YAML, \u0447\u0442\u043E \u0440\u0430\u0441\u0448\u0438\u0444\u0440\u043E\
  \u0432\u044B\u0432\u0430\u0435\u0442\u0441\u044F \u043A\u0430\u043A \"YAML Ain't\
  \ Markup Language\" (YAML - \u044D\u0442\u043E \u043D\u0435 \u044F\u0437\u044B\u043A\
  \ \u0440\u0430\u0437\u043C\u0435\u0442\u043A\u0438), \u044F\u0432\u043B\u044F\u0435\
  \u0442\u0441\u044F \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043E\u043C\
  \ \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438 \u0434\
  \u0430\u043D\u043D\u044B\u0445, \u043F\u0440\u0435\u0434\u043D\u0430\u0437\u043D\
  \u0430\u0447\u0435\u043D\u043D\u044B\u043C \u0434\u043B\u044F\u2026"
lastmod: 2024-02-19 22:05:03.968724
model: gpt-4-0125-preview
summary: "YAML, \u0447\u0442\u043E \u0440\u0430\u0441\u0448\u0438\u0444\u0440\u043E\
  \u0432\u044B\u0432\u0430\u0435\u0442\u0441\u044F \u043A\u0430\u043A \"YAML Ain't\
  \ Markup Language\" (YAML - \u044D\u0442\u043E \u043D\u0435 \u044F\u0437\u044B\u043A\
  \ \u0440\u0430\u0437\u043C\u0435\u0442\u043A\u0438), \u044F\u0432\u043B\u044F\u0435\
  \u0442\u0441\u044F \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043E\u043C\
  \ \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438 \u0434\
  \u0430\u043D\u043D\u044B\u0445, \u043F\u0440\u0435\u0434\u043D\u0430\u0437\u043D\
  \u0430\u0447\u0435\u043D\u043D\u044B\u043C \u0434\u043B\u044F\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
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
