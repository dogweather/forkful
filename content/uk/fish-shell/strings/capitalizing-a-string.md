---
title:                "Зробити першу літеру рядка великою"
aliases: - /uk/fish-shell/capitalizing-a-string.md
date:                  2024-02-03T19:05:37.509109-07:00
model:                 gpt-4-0125-preview
simple_title:         "Зробити першу літеру рядка великою"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Велика буква у рядку означає зміну так, що перша літера переводиться у верхній регістр, а решта рядка - у нижній. Це поширена задача при обробці тексту, нормалізації введення користувача та форматуванні даних для забезпечення послідовності або задоволення конкретних критеріїв формату.

## Як зробити:

У Fish Shell рядки можна маніпулювати безпосередньо з вбудованими функціями, без потреби в зовнішніх інструментах або бібліотеках. Щоб зробити першу літеру рядка великою, можна комбінувати команду `string` з підкомандами.

```fish
# Приклад рядка
set sample_string "hello world"

# Зробити першу літеру великою
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

Вивід:
```
Hello world
```

Для сценаріїв, що вимагають великих літер у кількох словах рядка (наприклад, перетворення "hello world" на "Hello World"), ви б ітерували по кожному слову, застосовуючи логіку великої букви до кожного:

```fish
# Приклад речення
set sentence "hello fish shell programming"

# Зробити першу літеру кожного слова великою
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# Об'єднати слова з великою буквою
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

Вивід:
```
Hello Fish Shell Programming
```

Зауважте, що Fish Shell не пропонує безпосереднього підходу однією командою для повної капіталізації речення так, як це роблять деякі мови програмування з їх методами рядків. Тому, комбінація `string split`, `string sub`, `string upper`, а потім знову об'єднання представляє ідіоматичний підхід у Fish Shell для досягнення цього.
