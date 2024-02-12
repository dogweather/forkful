---
title:                "Склеивание строк"
date:                  2024-01-28T23:56:24.401571-07:00
model:                 gpt-4-0125-preview
simple_title:         "Склеивание строк"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/fish-shell/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Конкатенация строк означает их соединение конец с концом. Программисты делают это для объединения текста, например, для создания полного предложения из слов или создания путей к файлам.

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
