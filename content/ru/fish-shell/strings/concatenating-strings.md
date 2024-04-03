---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:24.401571-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Fish \u0441\u043A\u043B\u0435\u0438\u0432\u0430\u0439\u0442\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0441 \u043F\u0440\u043E\u0431\u0435\
  \u043B\u0430\u043C\u0438 \u043C\u0435\u0436\u0434\u0443 \u043D\u0438\u043C\u0438\
  \ \u0438\u043B\u0438 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\
  \u0435 `string join`."
lastmod: '2024-03-13T22:44:45.821533-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Fish \u0441\u043A\u043B\u0435\u0438\u0432\u0430\u0439\u0442\u0435\
  \ \u0441\u0442\u0440\u043E\u043A\u0438 \u0441 \u043F\u0440\u043E\u0431\u0435\u043B\
  \u0430\u043C\u0438 \u043C\u0435\u0436\u0434\u0443 \u043D\u0438\u043C\u0438 \u0438\
  \u043B\u0438 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435\
  \ `string join`."
title: "\u0421\u043A\u043B\u0435\u0438\u0432\u0430\u043D\u0438\u0435 \u0441\u0442\u0440\
  \u043E\u043A"
weight: 3
---

## Как это сделать:
В Fish склеивайте строки с пробелами между ними или используйте `string join`.

```fish
# Соединяем 'Hello' и 'World!' с пробелом
echo 'Hello' 'World!'

# Вывод: Hello World!

# Конкатенация переменных
set greet "Howdy"
set who "Partner"
echo $greet $who

# Вывод: Howdy Partner

# Конкатенация без пробелов с помощью string join
set file "report"
set ext "txt"
string join '' $file '.' $ext

# Вывод: report.txt
```

## Подробнее
Конкатенация существует с зари программирования. В Fish `string join` проще, чем старые методы, такие как использование `echo`, за которым следуют строковые переменные без кавычек. Этот подход избегает нагрузки на подкоманды, что может быть выигрышем в производительности.

Альтернативы включают использование `printf`, которое предоставляет больший контроль над форматированием, но является немного более сложным для простых операций соединения. Пример:

```fish
set firstName "Ada"
set lastName "Lovelace"
printf "%s %s\n" $firstName $lastName
```

Команда `string` в Fish является частью встроенного набора инструментов для манипуляций со строками, введенного для упрощения обработки текста. Это не уникально для Fish, но её включение в качестве встроенного инструмента упрощает использование.

## Смотрите также
- Официальная документация Fish: [ссылка](https://fishshell.com/docs/current/cmds/string.html)
- Учебные материалы сообщества: [ссылка](https://fishshell.com/docs/current/tutorial.html#tutorial)
- Обсуждение манипулирования строками в оболочках: [ссылка](https://unix.stackexchange.com/questions/131766/why-does-my-shell-script-choke-on-whitespace-or-other-special-characters)
