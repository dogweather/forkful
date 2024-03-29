---
date: 2024-01-20 17:53:35.257739-07:00
description: "Debugging - \u0446\u0435 \u044F\u043A \u0434\u0435\u0442\u0435\u043A\
  \u0442\u0438\u0432\u043D\u0430 \u0440\u043E\u0431\u043E\u0442\u0430. \u0422\u0440\
  \u0435\u0431\u0430 \u0448\u0443\u043A\u0430\u0442\u0438 \u0445\u0442\u043E, \u0434\
  \u0435, \u0456 \u043A\u043E\u043B\u0438 \u043D\u0430\u043F\u043B\u0443\u0442\u0430\
  \u0432 \u0443 \u0432\u0430\u0448\u043E\u043C\u0443 \u043A\u043E\u0434\u0456. \u0412\
  \u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u0434\u0435\u0431\u0430\u0433\
  -\u0456\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457 \u0434\u043E\u043F\
  \u043E\u043C\u0430\u0433\u0430\u0454 \u0437\u0440\u043E\u0437\u0443\u043C\u0456\u0442\
  \u0438, \u0449\u043E \u0441\u0430\u043C\u0435 \u0440\u043E\u0431\u0438\u0442\u044C\
  \u2026"
lastmod: '2024-03-13T22:44:50.231640-06:00'
model: gpt-4-1106-preview
summary: "Debugging - \u0446\u0435 \u044F\u043A \u0434\u0435\u0442\u0435\u043A\u0442\
  \u0438\u0432\u043D\u0430 \u0440\u043E\u0431\u043E\u0442\u0430. \u0422\u0440\u0435\
  \u0431\u0430 \u0448\u0443\u043A\u0430\u0442\u0438 \u0445\u0442\u043E, \u0434\u0435\
  , \u0456 \u043A\u043E\u043B\u0438 \u043D\u0430\u043F\u043B\u0443\u0442\u0430\u0432\
  \ \u0443 \u0432\u0430\u0448\u043E\u043C\u0443 \u043A\u043E\u0434\u0456. \u0412\u0438\
  \u0432\u0435\u0434\u0435\u043D\u043D\u044F \u0434\u0435\u0431\u0430\u0433-\u0456\
  \u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457 \u0434\u043E\u043F\u043E\u043C\
  \u0430\u0433\u0430\u0454 \u0437\u0440\u043E\u0437\u0443\u043C\u0456\u0442\u0438\
  , \u0449\u043E \u0441\u0430\u043C\u0435 \u0440\u043E\u0431\u0438\u0442\u044C\u2026"
title: "\u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043D\u0430\u043B\u0430\
  \u0433\u043E\u0434\u0436\u0443\u0432\u0430\u043B\u044C\u043D\u043E\u0457 \u0456\u043D\
  \u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Debugging - це як детективна робота. Треба шукати хто, де, і коли наплутав у вашому коді. Виведення дебаг-інформації допомагає зрозуміти, що саме робить код і чому щось іде не так, як ми планували.

## Як це зробити:
```Ruby
# Простий вивід інформації до консолі
puts "Це повідомлення виводиться на екран"

# Щось пошло не так? Подивимось, що в змінній:
debug_variable = "Щось підозріле"
p debug_variable

# Хочете більше деталей? Використайте pp для красивого виводу:
require 'pp'
complex_variable = { a: 1, b: "text", c: [1, 2, 3] }
pp complex_variable
```

Простою мовою, замість загадок – чіткі відповіді прямо в терміналі.

## Пірнемо глибше:
Раніше, коли Ruby тільки з’являвся, не було стільки інструментів для дебагінгу. Виведення інформації у консоль було ключем до розуміння проблем. І досі це ефективний спосіб. Альтернативою є використання інструментів як IRB або Pry, де можна в реальному часі експериментувати з кодом. Деталі реалізації варіюються від простого `puts` до використання бібліотек, що дозволяють більше контролю, таких як `logger`.

## Дивіться також:
- [Ruby Doc - Kernel#puts](https://ruby-doc.org/core-3.1.0/Kernel.html#method-i-puts)
- [Ruby Doc - Kernel#p](https://ruby-doc.org/core-3.1.0/Kernel.html#method-i-p)
- [Ruby Doc - PP#pp](https://ruby-doc.org/stdlib-3.1.0/libdoc/pp/rdoc/PP.html#method-c-pp)
- [Pry documentation](https://github.com/pry/pry) - мощна альтернатива IRB для дебагінгу.
- [Logger Class](https://ruby-doc.org/stdlib-3.1.0/libdoc/logger/rdoc/Logger.html) - якщо вам потрібна більш гнучка система логування.
