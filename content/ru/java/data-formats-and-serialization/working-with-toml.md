---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:53.031415-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u0430\u043C \u043F\u043E\u043D\u0430\u0434\u043E\u0431\u0438\
  \u0442\u0441\u044F \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430\
  \ \u0434\u043B\u044F \u0440\u0430\u0437\u0431\u043E\u0440\u0430 TOML. \u042F \u0440\
  \u0435\u043A\u043E\u043C\u0435\u043D\u0434\u0443\u044E `toml4j`. \u0414\u043E\u0431\
  \u0430\u0432\u044C\u0442\u0435 \u0435\u0435 \u0432 \u0441\u0432\u043E\u0439 \u043F\
  \u0440\u043E\u0435\u043A\u0442 \u0442\u0430\u043A."
lastmod: '2024-03-13T22:44:44.865907-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u0430\u043C \u043F\u043E\u043D\u0430\u0434\u043E\u0431\u0438\u0442\
  \u0441\u044F \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430 \u0434\
  \u043B\u044F \u0440\u0430\u0437\u0431\u043E\u0440\u0430 TOML."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 TOML"
weight: 39
---

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
