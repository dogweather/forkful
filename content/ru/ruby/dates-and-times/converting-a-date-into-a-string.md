---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:52.499728-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Ruby \u0434\u0435\u043B\u0430\u0435\u0442 \u0438\u0433\u0440\u0443\
  \ \u0441 \u0434\u0430\u0442\u0430\u043C\u0438 \u0438 \u0441\u0442\u0440\u043E\u043A\
  \u0430\u043C\u0438 \u043E\u0447\u0435\u043D\u044C \u043F\u0440\u043E\u0441\u0442\
  \u043E\u0439. \u0412\u043E\u0442 \u043A\u0430\u043A \u044D\u0442\u043E \u0434\u0435\
  \u043B\u0430\u0435\u0442\u0441\u044F."
lastmod: '2024-03-13T22:44:46.018505-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u0434\u0435\u043B\u0430\u0435\u0442 \u0438\u0433\u0440\u0443 \u0441\
  \ \u0434\u0430\u0442\u0430\u043C\u0438 \u0438 \u0441\u0442\u0440\u043E\u043A\u0430\
  \u043C\u0438 \u043E\u0447\u0435\u043D\u044C \u043F\u0440\u043E\u0441\u0442\u043E\
  \u0439."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0434\u0430\u0442\u044B \u0432 \u0441\u0442\u0440\u043E\u043A\u0443"
weight: 28
---

## Как это сделать:
Ruby делает игру с датами и строками очень простой. Вот как это делается:

```Ruby
require 'date'

# Создаем объект дата
my_date = Date.new(2023, 4, 14)

# Преобразование в строку по умолчанию
date_string = my_date.to_s
puts date_string  # Вывод: "2023-04-14"

# Пользовательские форматы с помощью strftime (строковый формат времени)
pretty_date = my_date.strftime('%B %d, %Y')
puts pretty_date  # Вывод: "Апрель 14, 2023"

# Еще один пример, для интереса
fun_date_format = my_date.strftime('%d-%m-%Y')
puts fun_date_format  # Вывод: "14-04-2023"
```

## Подробнее
В былые времена люди писали дату от руки. В мире программирования класс `Date` в Ruby дал нам возможность обращаться с датами без лишних усилий. У вас есть методы, такие как `to_s` и `strftime`, чтобы превращать ваши объекты `Date` в строки.

Метод `to_s` дает вам быстрое представление в формате ISO 8601 (`YYYY-MM-DD`), что отлично подходит для преобразования без изысков. Но когда вам нужно, чтобы ваша дата выглядела нарядно, `strftime` позволяет выбрать точный шаблон, который будет следовать строка. Символы в `strftime` типа `%Y` для года из четырех цифр, `%m` для месяца из двух цифр и `%d` для дня из двух цифр являются вашими строительными блоками для форматирования дат.

Хотя классы `Date` и `Time` в Ruby надежны, гемы, такие как `Timecop` для путешествия во времени (не настоящего путешествия во времени, извините) во время тестов, или `Chronic` для анализа дат, написанных на естественном языке, могут добавить немного живости, когда вам это нужно.

Суть в чем? Ruby использует системные библиотеки—как части библиотек C, отвечающие за время—под капотом. Это означает, что оно быстродейственное и надежное, умело обращаясь с такими нюансами, как високосные годы и переход на летнее время.

## Смотрите также
Посмотрите эти ресурсы для получения дополнительной информации:
- Документация по классу `Date` Ruby: [ruby-doc.org/stdlib-2.7.3/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-3.1.2/libdoc/date/rdoc/Date.html)
- Директивы `strftime` в Ruby: [apidock.com/ruby/DateTime/strftime](https://apidock.com/ruby/DateTime/strftime)
- Гемы для еще большего магии с датой/временем: [github.com/travisjeffery/timecop](https://github.com/travisjeffery/timecop) и [github.com/mojombo/chronic](https://github.com/mojombo/chronic)
