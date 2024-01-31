---
title:                "Робота з YAML"
date:                  2024-01-19
simple_title:         "Робота з YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що та Чому?
YAML - це людино-читабельний формат для серіалізації даних, що часто застосовується для конфігураційних файлів. Програмісти використовують його через лаконічність та зручність зливання з системами контролю версій.

## Як це зробити:
Для роботи з YAML у Kotlin, використовуємо бібліотеку `snakeyaml`. Давайте парсити YAML:

```Kotlin
import org.yaml.snakeyaml.Yaml
import java.io.InputStream

fun main() {
    val yaml = Yaml()
    val inputStream: InputStream = this.javaClass
       .classLoader
       .getResourceAsStream("config.yaml")!!
    val data: Map<String, Any> = yaml.load(inputStream)
    
    println(data["name"]) // Виводить: Ваш проект
}

```
Вміст `config.yaml`:
```yaml
name: Ваш проект
version: 1.0.0
```

При запуску програми отримаємо вивід: Ваш проект.

## Більше інформації:
YAML виник у 2001 році. Популярні альтернативи - JSON і XML. Використання YAML у Kotlin потребує зовнішньої бібліотеки; основні варіанти - `snakeyaml` і `kotlinx.serialization`. `Snakeyaml` повністю реалізує специфікацію YAML 1.1, тоді як `kotlinx.serialization` пропонує більшу інтеграцію з Kotlin.

## Дивіться також:
- Офіційний сайт YAML: [yaml.org](https://yaml.org/)
- Документація бібліотеки Snakeyaml: [Snakeyaml wiki](https://bitbucket.org/asomov/snakeyaml/wiki/Documentation)
- Kotlin Serialization Guide: [Kotlinx.serialization](https://github.com/Kotlin/kotlinx.serialization)
