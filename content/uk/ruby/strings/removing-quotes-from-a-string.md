---
date: 2024-01-26 03:41:58.776604-07:00
description: "\u042F\u043A: Ruby \u043C\u0430\u0454 \u0434\u0435\u044F\u043A\u0456\
  \ \u043A\u043B\u0430\u0441\u043D\u0456 \u0442\u0440\u044E\u043A\u0438 \u0432 \u0440\
  \u0443\u043A\u0430\u0432\u0456 \u0434\u043B\u044F \u0432\u0438\u0440\u0456\u0437\
  \u0430\u043D\u043D\u044F \u0446\u0438\u0445 \u0434\u043E\u043A\u0443\u0447\u043B\
  \u0438\u0432\u0438\u0445 \u043B\u0430\u043F\u043E\u043A. \u0412\u0438 \u043C\u043E\
  \u0436\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u0432\u0430\u0442\u0438 \u043C\u0435\u0442\u043E\u0434\u0438 `gsub` \u0430\
  \u0431\u043E `delete`, \u0449\u043E\u0431 \u0432\u0438\u043A\u043E\u043D\u0430\u0442\
  \u0438 \u0440\u043E\u0431\u043E\u0442\u0443.\u2026"
lastmod: '2024-03-13T22:44:50.202479-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u043C\u0430\u0454 \u0434\u0435\u044F\u043A\u0456 \u043A\u043B\u0430\
  \u0441\u043D\u0456 \u0442\u0440\u044E\u043A\u0438 \u0432 \u0440\u0443\u043A\u0430\
  \u0432\u0456 \u0434\u043B\u044F \u0432\u0438\u0440\u0456\u0437\u0430\u043D\u043D\
  \u044F \u0446\u0438\u0445 \u0434\u043E\u043A\u0443\u0447\u043B\u0438\u0432\u0438\
  \u0445 \u043B\u0430\u043F\u043E\u043A."
title: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u043B\u0430\u043F\u043E\
  \u043A \u0437\u0456 \u0441\u0442\u0440\u043E\u043A\u0438"
weight: 9
---

## Як:
Ruby має деякі класні трюки в рукаві для вирізання цих докучливих лапок. Ви можете використовувати методи `gsub` або `delete`, щоб виконати роботу. Ось деякий код, на який варто звернути увагу:

```ruby
# Використання gsub для видалення подвійних та одинарних лапок
quoted_string = "\"Say 'hello' to my little friend!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# Вивід: Say hello to my little friend!

# Якщо ви знаєте, що матимете справу лише з одним типом лапок
single_quoted_string = "'Stay a while and listen!'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# Вивід: Stay a while and listen!
```

## Поглиблений огляд
Історія лапок веде свій початок з найраніших днів програмування, де вони часто виступали як розділювачі рядків. Наразі, як і тоді, вам може знадобитися видалити ці символи лапок, коли вони не потрібні або коли вони могли б втрутитися в зберігання та маніпулювання даними.

Ми говорили про `gsub` та `delete`, але є й інші методи, такі як `tr` або `tr_s`, які дають вам трохи більше контролю або можуть обробляти деякі інші випадки використання:

```ruby
# tr теж може видаляти лапки
double_quoted_string = "\"Do or do not, there is no try.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# Вивід: Do or do not, there is no try.
```

Пам'ятайте, кожен з цих методів має свої випадки використання. `gsub` є потужнішим, коли ви маєте справу зі складними шаблонами або множинними замінами. `delete` та `tr` чудово працюють для простого, прямолінійного видалення символів.

## Дивіться також
Для додаткового читання та перегляду цих методів на практиці у більших кодових базах, ознайомтесь з:
- Документацією Ruby для [String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub), [String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete), та [String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr).
- Ruby Monstas має чудовий [набір вправ на рядки](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html), який включає роботу з лапками.
- Дискусії на Stack Overflow по [маніпулюванню рядками](https://stackoverflow.com/search?q=ruby+remove+quotes+from+string) надають реальні проблеми та рішення від спільноти Ruby.
