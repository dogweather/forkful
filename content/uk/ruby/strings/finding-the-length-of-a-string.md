---
date: 2024-01-20 17:48:27.762525-07:00
description: "\u041E\u0446\u0456\u043D\u043A\u0430 \u0434\u043E\u0432\u0436\u0438\u043D\
  \u0438 \u0440\u044F\u0434\u043A\u0430 - \u0446\u0435 \u043F\u0440\u043E \u0437'\u044F\
  \u0441\u0443\u0432\u0430\u043D\u043D\u044F \u043A\u0456\u043B\u044C\u043A\u043E\u0441\
  \u0442\u0456 \u0441\u0438\u043C\u0432\u043E\u043B\u0456\u0432 \u0443 \u043D\u044C\
  \u043E\u043C\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\
  \u0438 \u0446\u0435 \u0440\u043E\u0431\u043B\u044F\u0442\u044C, \u0449\u043E\u0431\
  \ \u043F\u0435\u0440\u0435\u0432\u0456\u0440\u0438\u0442\u0438 \u0432\u0432\u043E\
  \u0434 \u0432\u0456\u0434 \u043A\u043E\u0440\u0438\u0441\u0442\u0443\u0432\u0430\
  \u0447\u0430, \u043E\u0431\u043C\u0435\u0436\u0438\u0442\u0438 \u0442\u0435\u043A\
  \u0441\u0442 \u0432 UI, \u0447\u0438\u2026"
lastmod: '2024-03-13T22:44:50.208057-06:00'
model: gpt-4-1106-preview
summary: "\u041E\u0446\u0456\u043D\u043A\u0430 \u0434\u043E\u0432\u0436\u0438\u043D\
  \u0438 \u0440\u044F\u0434\u043A\u0430 - \u0446\u0435 \u043F\u0440\u043E \u0437'\u044F\
  \u0441\u0443\u0432\u0430\u043D\u043D\u044F \u043A\u0456\u043B\u044C\u043A\u043E\u0441\
  \u0442\u0456 \u0441\u0438\u043C\u0432\u043E\u043B\u0456\u0432 \u0443 \u043D\u044C\
  \u043E\u043C\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\
  \u0438 \u0446\u0435 \u0440\u043E\u0431\u043B\u044F\u0442\u044C, \u0449\u043E\u0431\
  \ \u043F\u0435\u0440\u0435\u0432\u0456\u0440\u0438\u0442\u0438 \u0432\u0432\u043E\
  \u0434 \u0432\u0456\u0434 \u043A\u043E\u0440\u0438\u0441\u0442\u0443\u0432\u0430\
  \u0447\u0430, \u043E\u0431\u043C\u0435\u0436\u0438\u0442\u0438 \u0442\u0435\u043A\
  \u0441\u0442 \u0432 UI, \u0447\u0438\u2026"
title: "\u0412\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u0434\u043E\u0432\
  \u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Оцінка довжини рядка - це про з'ясування кількості символів у ньому. Програмісти це роблять, щоб перевірити ввод від користувача, обмежити текст в UI, чи просто тому, що робота з текстовими даними цього вимагає.

## Як це зробити:
```ruby
# Простий спосіб отримати довжину рядка у Ruby:
message = "Привіт, світе!"
puts message.length
# Вивід: 14

# Альтернативний метод:
puts message.size
# Вивід: 14
```

## Пірнання у глибину
Наш метод `.length` існує як синонім до `.size` у Ruby майже з самого старту, пропонуючи обидва варіанти, щоб було зручніше для тих, хто переходить з інших мов програмування. Історично, такі методи стали необхідною частиною мов умовляючи з появою потреби у обробці рядків та текстових файлів.

Обидва методи повертають кількість символів у рядку, і в UTF-8, один символ може складатися з декількох байтів. Це важливо, бо на відміну від ASCII де кожний символ це один байт, у UTF-8 символи різних мов можуть мати різну байтову довжину. Ruby гарно впорається з цим, показуючи, наскільки сучасні мови програмування адаптовані до глобалізації.

Є і рідше вживані методи, як наприклад `.bytesize`, який показує довжину рядка у байтах, не символах, що може бути корисно при роботі з мережевими протоколами та збереженням даних.

## Дивіться також
- [Ruby Документація для String](https://ruby-doc.org/core-3.1.0/String.html)
- [UTF-8 та що почитати про кодування символів](https://uk.wikipedia.org/wiki/UTF-8)
