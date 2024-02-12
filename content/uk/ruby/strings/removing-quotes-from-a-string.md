---
title:                "Видалення лапок зі строки"
aliases:
- /uk/ruby/removing-quotes-from-a-string/
date:                  2024-01-26T03:41:58.776604-07:00
model:                 gpt-4-0125-preview
simple_title:         "Видалення лапок зі строки"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Що та Чому?
Видалення лапок з рядка означає зняття цих подвійних або одинарних лапок, які обгортають текстові значення. Програмісти часто роблять це для очищення вводу користувача, забезпечення послідовності в обробці даних або підготовки даних для систем, які можуть сплутати з цими зайвими символами.

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
