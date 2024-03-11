---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:53.031415-07:00
description: "TOML \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442 \"Tom's Obvious,\
  \ Minimal Language\" (\u0422\u043E\u043C\u043E\u0432 \u042F\u0432\u043D\u044B\u0439\
  , \u041C\u0438\u043D\u0438\u043C\u0430\u043B\u044C\u043D\u044B\u0439 \u042F\u0437\
  \u044B\u043A). \u042D\u0442\u043E \u0444\u043E\u0440\u043C\u0430\u0442 \u0441\u0435\
  \u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438 \u0434\u0430\u043D\u043D\
  \u044B\u0445, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C\u044B\
  \u0439 \u0434\u043B\u044F \u0444\u0430\u0439\u043B\u043E\u0432 \u043A\u043E\u043D\
  \u0444\u0438\u0433\u0443\u0440\u0430\u0446\u0438\u0438.\u2026"
lastmod: '2024-03-11T00:14:18.470813-06:00'
model: gpt-4-0125-preview
summary: "TOML \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442 \"Tom's Obvious, Minimal\
  \ Language\" (\u0422\u043E\u043C\u043E\u0432 \u042F\u0432\u043D\u044B\u0439, \u041C\
  \u0438\u043D\u0438\u043C\u0430\u043B\u044C\u043D\u044B\u0439 \u042F\u0437\u044B\u043A\
  ). \u042D\u0442\u043E \u0444\u043E\u0440\u043C\u0430\u0442 \u0441\u0435\u0440\u0438\
  \u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438 \u0434\u0430\u043D\u043D\u044B\u0445\
  , \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C\u044B\u0439 \u0434\
  \u043B\u044F \u0444\u0430\u0439\u043B\u043E\u0432 \u043A\u043E\u043D\u0444\u0438\
  \u0433\u0443\u0440\u0430\u0446\u0438\u0438.\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 TOML"
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
