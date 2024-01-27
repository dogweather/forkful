---
title:                "Використання регулярних виразів"
date:                  2024-01-19
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Регулярні вирази (regex) дозволяють шукати шаблони в тексті. Програмісти використовують їх для валідації, пошуку, заміни тексту - швидко і гнучко.

## Як на те йти:
```Ruby
# Пошук слова 'котик'
text = "У мене є котик і песик"
pattern = /котик/
match = text.match(pattern)
puts match[0]  # Виведе 'котик', якщо знайдено

# Заміна 'котик' на 'кіт'
new_text = text.gsub(/котик/, 'кіт')
puts new_text  # Виведе 'У мене є кіт і песик'

# Валідація формату email
email = "test@example.com"
email_pattern = /\A[\w+\-.]+@[a-z\d\-.]+\.[a-z]+\z/i
valid = email.match(email_pattern) ? "Так" : "Ні"
puts "Чи валідний email? #{valid}"  # Виведе 'Чи валідний email? Так'
```

## Пірнаємо глибше:
Регулярні вирази започатковані у 1950-х, сильно еволюціонували. Є альтернативи, які parsers чи string methods, але вони часто повільніші або менш потужні. В Ruby регулярні вирази реалізовані через бібліотеку Oniguruma, що надає гнучкість і швидкість.

## Ось ще кілька посилань:
- [Ruby-Doc Regexp](https://ruby-doc.org/core-3.1.0/Regexp.html)
- [Rubular: регулярні вирази тестер](http://rubular.com/)
- [Learn Ruby Regexp](https://learn.co/lessons/regex-class-methods-readme)
