---
title:                "Работа с TOML"
aliases:
- /ru/java/working-with-toml/
date:                  2024-01-29T00:04:53.031415-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/working-with-toml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
TOML означает "Tom's Obvious, Minimal Language" (Томов Явный, Минимальный Язык). Это формат сериализации данных, используемый для файлов конфигурации. Программисты используют его, потому что он легко читаем, пишется и хорошо соотносится с хеш-таблицей.

## Как это сделать:
Вам понадобится библиотека для разбора TOML. Я рекомендую `toml4j`. Добавьте ее в свой проект так:

```java
// Добавьте это в ваш build.gradle
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

Вот как вы можете разобрать файл TOML:

```java
import com.moandjiezana.toml.Toml;

public class TomlExample {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("server.ip");
        Integer port = toml.getLong("server.port").intValue();
        
        System.out.println("IP сервера: " + ip);
        System.out.println("Порт сервера: " + port);
    }
}
```

Пример вывода:

```
IP сервера: 192.168.1.1
Порт сервера: 80
```

## Подробнее
Разработанный сооснователем GitHub Томом Престон-Вернером, TOML был направлен на то, чтобы быть проще, чем XML, и более специфическим, чем YAML. Его последняя версия 1.0.0, выпущенная в 2021 году, предлагает стабильный набор функций.

Такие альтернативы, как JSON или YAML, также популярны. JSON отлично подходит для обмена данными. YAML более читабельный для сложных конфигураций. Сила TOML в его прямолинейности и использовании в сообществе Rust.

Что касается реализации, когда вы используете TOML с Java, помните, что выбор парсера имеет значение. Помимо `toml4j`, некоторые выбирают `jackson-dataformat-toml`. У каждого будут свои особенности, такие как обработка ошибок или производительность разбора, поэтому выбирайте в зависимости от потребностей вашего проекта.

## Смотрите также
- Спецификация TOML: https://toml.io/en/
- `toml4j` на GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
