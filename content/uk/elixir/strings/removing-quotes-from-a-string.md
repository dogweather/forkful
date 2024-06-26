---
date: 2024-01-26 03:39:26.241430-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Elixir \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\
  \u043D\u043E\u0457 \u0444\u0443\u043D\u043A\u0446\u0456\u0457 \u0434\u043B\u044F\
  \ \u0432\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u043B\u0430\u043F\u043E\
  \u043A, \u0430\u043B\u0435 \u0441\u0442\u0432\u043E\u0440\u0438\u0442\u0438 \u0432\
  \u043B\u0430\u0441\u043D\u0443 \u0434\u043E\u0441\u0438\u0442\u044C \u043F\u0440\
  \u043E\u0441\u0442\u043E \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\
  \u043E\u044E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F\
  \ \u0437\u0456\u0441\u0442\u0430\u0432\u043B\u0435\u043D\u043D\u044F \u0432\u0456\
  \u0437\u0435\u0440\u0443\u043D\u043A\u0456\u0432\u2026"
lastmod: '2024-03-13T22:44:48.702915-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u043E\u0457 \u0444\u0443\u043D\u043A\u0446\u0456\u0457 \u0434\u043B\
  \u044F \u0432\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u043B\u0430\u043F\
  \u043E\u043A, \u0430\u043B\u0435 \u0441\u0442\u0432\u043E\u0440\u0438\u0442\u0438\
  \ \u0432\u043B\u0430\u0441\u043D\u0443 \u0434\u043E\u0441\u0438\u0442\u044C \u043F\
  \u0440\u043E\u0441\u0442\u043E \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\
  \u0433\u043E\u044E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\
  \u044F \u0437\u0456\u0441\u0442\u0430\u0432\u043B\u0435\u043D\u043D\u044F \u0432\
  \u0456\u0437\u0435\u0440\u0443\u043D\u043A\u0456\u0432 \u0430\u0431\u043E \u0444\
  \u0443\u043D\u043A\u0446\u0456\u0439 `String`."
title: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u043B\u0430\u043F\u043E\
  \u043A \u0437\u0456 \u0441\u0442\u0440\u043E\u043A\u0438"
weight: 9
---

## Як це зробити:
Elixir не має вбудованої функції для видалення лапок, але створити власну досить просто за допомогою використання зіставлення візерунків або функцій `String`. Ось ці фрагменти:

```elixir
# Використання зіставлення візерунків
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# Приклад використання
unquote_string("\"Привіт, Світе!\"") # => "Привіт, Світе!"
unquote_string("'Привіт, Світе!'")   # => "Привіт, Світе!"

# Використання String.trim/1
def unquote_string(string), do: String.trim(string, "'\"")

# Приклад використання
unquote_string("\"Привіт, Світе!\"") # => "Привіт, Світе!"
unquote_string("'Привіт, Світе!'")   # => "Привіт, Світе!"
```

Результат для обох методів буде:
```
"Привіт, Світе!"
```

## Поглиблений Дослід
У минулі часи, лапки у рядках були наче мінне поле—не розрахуєш правильно, і бац, синтаксичні помилки або діри у безпеці. У Elixir, зіставлення візерунків дозволяє обробляти ваші рядки як конструктор Lego, даючи вам можливість розбирати та знову зібрати з високою точністю. Його міцний модуль `String` також зручний, гнучко видаляючи лапки за допомогою функцій `trim`. Альтернативи? Регулярні вирази можуть викинути лапки геть, а зовнішні бібліотеки можуть надати додаткову вогневу потужність, якщо вам потрібно більше, ніж просте видалення.

## Дивіться також
Занурся глибше з цими ресурсами:
- [String модуль Elixir](https://hexdocs.pm/elixir/String.html)
- [Дізнайтеся більше про зіставлення візерунків у Elixir](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Регулярні вирази у Elixir (модуль Regex)](https://hexdocs.pm/elixir/Regex.html)
