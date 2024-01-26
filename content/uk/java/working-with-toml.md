---
title:                "Робота з TOML"
date:                  2024-01-26T04:23:28.028159-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/working-with-toml.md"
---

{{< edit_this_page >}}

## Що та Чому?
TOML означає "Tom's Obvious, Minimal Language" (Очевидна, Мінімалістична Мова Тома). Це формат серіалізації даних, що використовується для файлів конфігурації. Програмісти використовують його через те, що він легкий для читання, написання і гарно проектується на хеш-таблицю.

## Як це зробити:
Вам знадобиться бібліотека для розбору TOML. Я рекомендую `toml4j`. Додайте її до свого проєкту так:

```java
// Додайте це до вашого build.gradle
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

Ось як ви можете розібрати TOML файл:

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
        
        System.out.println("IP Сервера: " + ip);
        System.out.println("Порт Сервера: " + port);
    }
}
```

Приклад виводу:

```
IP Сервера: 192.168.1.1
Порт Сервера: 80
```

## Поглиблено
Розроблений співзасновником GitHub Томом Престон-Вернером, TOML мав на меті бути простішим за XML і більш спеціфікованим за YAML. Його остання версія 1.0.0, випущена у 2021 році, пропонує стабільний набір функцій.

Альтернативи, такі як JSON або YAML, також популярні. JSON чудово підходить для обміну даними. YAML більш зручний для читання у складних конфігураціях. Сила TOML полягає в його прямолінійності та використанні в спільноті Rust.

Що стосується імплементації, коли використовується TOML з Java, майте на увазі, що важливий вибір парсера. Крім `toml4j`, деякі вибирають `jackson-dataformat-toml`. Вони кожен матимуть свої нюанси, як-от обробка помилок або продуктивність розбору, тому обирайте залежно від потреб вашого проєкту.

## Дивіться також
- Специфікація TOML: https://toml.io/en/
- `toml4j` на GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml