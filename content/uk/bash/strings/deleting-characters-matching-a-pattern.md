---
title:                "Видалення символів за візерунком"
aliases:
- /uk/bash/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:41:59.490678-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Удалення символів за візерунком - це процес забирання певних символів з тексту. Програмісти роблять це для очищення даних, форматування або обмеження вводу.

## How to: (Як це зробити:)
```Bash
# Видалення всіх цифр з текстового рядка
echo "abc123def456" | tr -d '0-9'
# Вивід: abcdef

# Видалення конкретних символів
echo "hello, world!" | tr -d '!,'
# Вивід: hello world

# Видалення символів, що відповідають візерунку (приклад regex)
echo "foo123bar456" | sed 's/[0-9]//g'
# Вивід: foobar
```

## Deep Dive (Поглиблений розбір):
Коли мова заходить про Bash, історія йде глибоко в Unix, де текстова обробка завжди була ключовою. Команди `tr` та `sed` - класичні утиліти Unix, використовуються для обробки тексту з 70-х років. `tr` (translate або transmute) змінює або видаляє символи, `sed` (stream editor) - більш потужний і може використовувати регулярні вирази.

Поряд з ними, є сучасні інструменти, такі як `awk`, який також може обробляти тексти за складнішими правилами. Хоча `tr` є простішим, `sed` та `awk` пропонують велику гнучкість.

Що до видалення символів, така потреба часто виникає під час скриптинга, наприклад, коли потрібно обробити результати команд або виводу з програм.

## See Also (Додаткові матеріали):
- [GNU `tr` Manual](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [GNU `sed` Manual](https://www.gnu.org/software/sed/manual/sed.html)
- [Introduction to `awk`](https://www.ibm.com/docs/en/aix/7.2?topic=utilities-awk-programming-language)
