---
date: 2024-01-26 04:23:28.028159-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412\u0430\u043C \u0437\u043D\u0430\u0434\u043E\u0431\u0438\u0442\u044C\u0441\
  \u044F \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0430 \u0434\u043B\
  \u044F \u0440\u043E\u0437\u0431\u043E\u0440\u0443 TOML. \u042F \u0440\u0435\u043A\
  \u043E\u043C\u0435\u043D\u0434\u0443\u044E `toml4j`. \u0414\u043E\u0434\u0430\u0439\
  \u0442\u0435 \u0457\u0457 \u0434\u043E \u0441\u0432\u043E\u0433\u043E \u043F\u0440\
  \u043E\u0454\u043A\u0442\u0443 \u0442\u0430\u043A."
lastmod: '2024-03-13T22:44:49.119113-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u0430\u043C \u0437\u043D\u0430\u0434\u043E\u0431\u0438\u0442\u044C\
  \u0441\u044F \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0430 \u0434\
  \u043B\u044F \u0440\u043E\u0437\u0431\u043E\u0440\u0443 TOML."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML"
weight: 39
---

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
