---
title:                "Створення текстового файлу"
html_title:           "Lua: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що & Чому?
Запис текстового файлу це процес створення файлу, який містить текстову інформацію, зазвичай читабельну для людини. Програмісти часто записують текстові файли для збереження та обміну даними між програмами.

## Як це зробити:
Для створення текстового файлу використовуються наступні кроки:

```
-- Відкриття файлу для запису
file = io.open("myfile.txt", "w")

-- Запис тексту у файл
file:write("Привіт, це мій новий текстовий файл!")

-- Закриття файлу
file:close()
```

В результаті виконання цих кроків у вашій папці з'явиться новий файл з назвою "myfile.txt", в якому буде написаний текст "Привіт, це мій новий текстовий файл!".

## Глибоке поглиблення:
Запис текстових файлів є одним із найбільш поширених методів збереження даних у програмуванні. Цей процес походить зі старої практики збереження інформації на папері, коли програмісти ручно записували код своїх програм.

Існують також альтернативні методи запису текстових файлів, такі як використання баз даних або сервісів хмарного сховища. Однак, запис текстових файлів залишається широко використовуваним способом, оскільки це простий та швидкий спосіб збереження та обміну даними.

Щоб детальніше ознайомитися зі записом текстових файлів у Lua, можна прочитати документацію на офіційному сайті Lua, а також переглянути різноманітні приклади інших програмістів.

## Також подивіться:
- [Офіційна документація Lua](https://www.lua.org/)
- [Приклади запису текстових файлів у Lua](https://www.lua.org/pil/21.1.html)
- [Стаття про роботу з файлами у Lua](https://www.tutorialspoint.com/lua/lua_input_output.htm)