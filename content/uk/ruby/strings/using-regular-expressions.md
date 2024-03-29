---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:32.171027-07:00
description: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0456 \u0432\u0438\u0440\
  \u0430\u0437\u0438 (regex) \u0443 Ruby - \u0446\u0435 \u0448\u0430\u0431\u043B\u043E\
  \u043D\u0438, \u0449\u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u044E\u0442\u044C\u0441\u044F \u0434\u043B\u044F \u043F\u043E\u0448\
  \u0443\u043A\u0443 \u043A\u043E\u043C\u0431\u0456\u043D\u0430\u0446\u0456\u0439\
  \ \u0441\u0438\u043C\u0432\u043E\u043B\u0456\u0432 \u0443 \u0440\u044F\u0434\u043A\
  \u0430\u0445, \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u044E\u0447\u0438 \u0440\
  \u043E\u0437\u0440\u043E\u0431\u043D\u0438\u043A\u0430\u043C \u0435\u0444\u0435\u043A\
  \u0442\u0438\u0432\u043D\u043E \u0448\u0443\u043A\u0430\u0442\u0438,\u2026"
lastmod: '2024-03-13T22:44:50.206170-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0456 \u0432\u0438\u0440\
  \u0430\u0437\u0438 (regex) \u0443 Ruby - \u0446\u0435 \u0448\u0430\u0431\u043B\u043E\
  \u043D\u0438, \u0449\u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u044E\u0442\u044C\u0441\u044F \u0434\u043B\u044F \u043F\u043E\u0448\
  \u0443\u043A\u0443 \u043A\u043E\u043C\u0431\u0456\u043D\u0430\u0446\u0456\u0439\
  \ \u0441\u0438\u043C\u0432\u043E\u043B\u0456\u0432 \u0443 \u0440\u044F\u0434\u043A\
  \u0430\u0445, \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u044E\u0447\u0438 \u0440\
  \u043E\u0437\u0440\u043E\u0431\u043D\u0438\u043A\u0430\u043C \u0435\u0444\u0435\u043A\
  \u0442\u0438\u0432\u043D\u043E \u0448\u0443\u043A\u0430\u0442\u0438,\u2026"
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\
  \u0456\u0432"
---

{{< edit_this_page >}}

## Що та Чому?
Регулярні вирази (regex) у Ruby - це шаблони, що використовуються для пошуку комбінацій символів у рядках, дозволяючи розробникам ефективно шукати, знаходити відповідності та маніпулювати текстом. Програмісти використовують regex для завдань таких як валідація, парсинг та маніпулювання рядками, роблячи їх незамінним інструментом для обробки тексту.

## Як користуватися:
### Базовий пошук
Щоб знайти відповідність рядка з простим шаблоном, ви можете використати метод `match`. Нижче ми перевіряємо, чи існує слово "Ruby" у даному рядку.

```ruby
if /Ruby/.match("Hello, Ruby!")
  puts "Знайдено відповідність!"
end
# Вивід: Знайдено відповідність!
```

### Пошук за шаблоном із змінними
Ви можете інтерполювати змінні у ваш regex, використовуючи синтаксис `#{}`, що робить ваші шаблони динамічними.

```ruby
language = "Ruby"
if /#{language}/.match("Програмування на Ruby це весело.")
  puts "Говоримо про Ruby!"
end
# Вивід: Говоримо про Ruby!
```

### Використання Regex для Заміни
Метод `gsub` дозволяє замінювати кожну появу шаблону на вказаний рядок заміни.

```ruby
puts "foobarfoo".gsub(/foo/, "bar")
# Вивід: barbarbar
```

### Захоплення
Дужки в regex використовуються для захоплення частин відповідності. Метод `match` повертає об'єкт `MatchData`, який ви можете використовувати для доступу до захоплених даних.

```ruby
match_data = /(\w+): (\d+)/.match("Вік: 30")
puts match_data[1] # Захоплена мітка
puts match_data[2] # Захоплене значення
# Вивід:
# Вік
# 30
```

### Використання Сторонніх Бібліотек
Хоча стандартна бібліотека Ruby є потужною, інколи вам може знадобитись більш спеціалізована функціональність. Одним із популярних gem для роботи з regex є `Oniguruma`, що надає додаткові можливості regex, яких немає в вбудованому механізмі Ruby regex.

Встановити його можна, використовуючи:
```bash
gem install oniguruma
```

Приклад використання може виглядати так (за умови, що ви підключили `oniguruma` після його установки):

```ruby
# Це більш складний приклад, який може вимагати додаткового налаштування
require 'oniguruma'

pattern = Oniguruma::ORegexp.new('(\d+)')
match_data = pattern.match("Номер є 42.")
puts match_data[1]
# Вивід: 42
```

Пам'ятайте, незважаючи на потужність, регулярні вирази можуть стати складними та важкими для управління при роботі з більш складними шаблонами. Прагніть до зрозумілості та розглядайте альтернативні методи, якщо ваш regex стає надто заплутаним.
