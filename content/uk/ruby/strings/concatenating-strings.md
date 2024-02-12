---
title:                "Об'єднання рядків"
aliases: - /uk/ruby/concatenating-strings.md
date:                  2024-01-20T17:35:52.366511-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Конкатенація рядків у Ruby – це метод з'єднання двох або більше рядків разом. Програмісти це роблять, аби збирати текст з менших фрагментів – наприклад, для створення повідомлення або динамічних вмістів.

## Як це зробити:
```Ruby
# З'єднання рядків з допомогою `+`
hello = "Привіт, "
world = "світе!"
greeting = hello + world
puts greeting  # Вивід: Привіт, світе!

# Інтерполяція рядків з допомогою `#{}` у подвійних лапках
name = "Олег"
welcome_message = "Вітаю, #{name}!"
puts welcome_message  # Вивід: Вітаю, Олег!

# Додавання рядка до існуючого з `<<`
exclamation = "!"
greeting << exclamation
puts greeting  # Вивід: Привіт, світе!!
```

## Поглиблений Розбір:
У минулому, конкатенація рядків використовувалась у багатьох мовах програмування, а не лише Ruby. Однак, з часом виникли альтернативи. Крім методу `+`, в Ruby є метод `concat` та оператор `<<`, який називається швидшим, бо не створює додаткових рядків у пам'яті при кожному додаванні. Інтерполяція рядків – ще один спосіб, який дозволяє вставляти змінні безпосередньо в рядок, це зручно і забезпечує кращу читабельність.

## Додаткові Матеріали:
- Розділ про рядки у документації Ruby: [Ruby String Documentation](https://ruby-doc.org/core-3.1.0/String.html)
- Ruby Style Guide, який порушує питання стилю коду, включаючи роботу з рядками: [Ruby Style Guide](https://rubystyle.guide/#strings)
- Проекти з відкритим кодом, де можна побачити реальне використання конкатенації рядків: [Ruby on Rails](https://github.com/rails/rails), [Sinatra](https://github.com/sinatra/sinatra)
