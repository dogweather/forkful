---
title:                "Пошук та заміна тексту"
html_title:           "Ruby: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому
Редагування тексту є важливою частиною програмування. Пошук і заміна тексту дозволяє швидко змінювати і оновлювати великі обсяги коду за допомогою декількох простих кроків.

## Як
```Ruby
# Заміна всіх входжень "Київ" на "Львів" у рядку
text = "Київ - це столиця України. Київ - це прекрасне місто."
new_text = text.gsub("Київ", "Львів")
puts new_text
# Вивід: Львів - це столиця України. Львів - це прекрасне місто.

# Заміна першого входження "Ruby" на "Python" у рядку
text = "Ruby is a great programming language. Ruby is also used for web development."
new_text = text.sub("Ruby", "Python")
puts new_text
# Вивід: Python is a great programming language. Ruby is also used for web development.
```
Методи `gsub` та `sub` дозволяють шукати та замінювати текст у рядках за певними шаблонами. Метод `gsub` буде замінювати всі входження шаблону, тоді як метод `sub` - тільки перше входження.

## Deep Dive
Щоб краще розуміти роботу пошуку і заміни тексту в Ruby, можна вивчити про регулярні вирази. Вони дозволяють задавати більш специфічні шаблони для пошуку, наприклад, шукати тільки слова з певними символами або шаблонами. Існує багато онлайн-ресурсів, де можна вивчити основи регулярних виразів для Ruby, наприклад, [цей](https://www.rubyguides.com/2015/06/ruby-regex/) та [цей](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm).

## Дивіться також
- [Документація Ruby](https://www.ruby-lang.org/uk/documentation/)
- [Основи програмування на Ruby](https://rubyonrails.org/doctrine/rails/)
- [Спільнота розробників Ruby в Україні](https://ruby.org.ua/)