---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:37.509109-07:00
description: "\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: \u0423 Fish\
  \ Shell \u0440\u044F\u0434\u043A\u0438 \u043C\u043E\u0436\u043D\u0430 \u043C\u0430\
  \u043D\u0456\u043F\u0443\u043B\u044E\u0432\u0430\u0442\u0438 \u0431\u0435\u0437\u043F\
  \u043E\u0441\u0435\u0440\u0435\u0434\u043D\u044C\u043E \u0437 \u0432\u0431\u0443\
  \u0434\u043E\u0432\u0430\u043D\u0438\u043C\u0438 \u0444\u0443\u043D\u043A\u0446\u0456\
  \u044F\u043C\u0438, \u0431\u0435\u0437 \u043F\u043E\u0442\u0440\u0435\u0431\u0438\
  \ \u0432 \u0437\u043E\u0432\u043D\u0456\u0448\u043D\u0456\u0445 \u0456\u043D\u0441\
  \u0442\u0440\u0443\u043C\u0435\u043D\u0442\u0430\u0445 \u0430\u0431\u043E \u0431\
  \u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0430\u0445. \u0429\u043E\u0431\
  \ \u0437\u0440\u043E\u0431\u0438\u0442\u0438\u2026"
lastmod: '2024-03-13T22:44:50.037358-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Fish Shell \u0440\u044F\u0434\u043A\u0438 \u043C\u043E\u0436\u043D\
  \u0430 \u043C\u0430\u043D\u0456\u043F\u0443\u043B\u044E\u0432\u0430\u0442\u0438\
  \ \u0431\u0435\u0437\u043F\u043E\u0441\u0435\u0440\u0435\u0434\u043D\u044C\u043E\
  \ \u0437 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0438\u043C\u0438 \u0444\
  \u0443\u043D\u043A\u0446\u0456\u044F\u043C\u0438, \u0431\u0435\u0437 \u043F\u043E\
  \u0442\u0440\u0435\u0431\u0438 \u0432 \u0437\u043E\u0432\u043D\u0456\u0448\u043D\
  \u0456\u0445 \u0456\u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u0430\u0445\
  \ \u0430\u0431\u043E \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0430\
  \u0445."
title: "\u0417\u0440\u043E\u0431\u0438\u0442\u0438 \u043F\u0435\u0440\u0448\u0443\
  \ \u043B\u0456\u0442\u0435\u0440\u0443 \u0440\u044F\u0434\u043A\u0430 \u0432\u0435\
  \u043B\u0438\u043A\u043E\u044E"
weight: 2
---

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
