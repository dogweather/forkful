---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:10.787190-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u0444\u0430\u0439\u043B\
  \u0430\u043C\u0438 CSV (\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F, \u0440\
  \u043E\u0437\u0434\u0456\u043B\u0435\u043D\u0456 \u043A\u043E\u043C\u0430\u043C\u0438\
  ) \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454 \u043F\u0430\u0440\
  \u0441\u0438\u043D\u0433, \u043C\u0430\u043D\u0456\u043F\u0443\u043B\u044E\u0432\
  \u0430\u043D\u043D\u044F \u0442\u0430 \u0433\u0435\u043D\u0435\u0440\u0430\u0446\
  \u0456\u044E \u0434\u0430\u043D\u0438\u0445 \u0443 \u0442\u0430\u0431\u043B\u0438\
  \u0447\u043D\u043E\u043C\u0443 \u0444\u043E\u0440\u043C\u0430\u0442\u0456, \u0449\
  \u043E \u0448\u0438\u0440\u043E\u043A\u043E \u0432\u0438\u043A\u043E\u0440\u0438\
  \u0441\u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F\u2026"
lastmod: '2024-03-13T22:44:50.107767-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u0444\u0430\u0439\u043B\u0430\
  \u043C\u0438 CSV (\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F, \u0440\u043E\
  \u0437\u0434\u0456\u043B\u0435\u043D\u0456 \u043A\u043E\u043C\u0430\u043C\u0438\
  ) \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454 \u043F\u0430\u0440\
  \u0441\u0438\u043D\u0433, \u043C\u0430\u043D\u0456\u043F\u0443\u043B\u044E\u0432\
  \u0430\u043D\u043D\u044F \u0442\u0430 \u0433\u0435\u043D\u0435\u0440\u0430\u0446\
  \u0456\u044E \u0434\u0430\u043D\u0438\u0445 \u0443 \u0442\u0430\u0431\u043B\u0438\
  \u0447\u043D\u043E\u043C\u0443 \u0444\u043E\u0440\u043C\u0430\u0442\u0456, \u0449\
  \u043E \u0448\u0438\u0440\u043E\u043A\u043E \u0432\u0438\u043A\u043E\u0440\u0438\
  \u0441\u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
---

{{< edit_this_page >}}

## Що та чому?

Робота з файлами CSV (значення, розділені комами) передбачає парсинг, маніпулювання та генерацію даних у табличному форматі, що широко використовується для обміну даними між застосунками. Програмісти виконують ці операції для ефективної обробки та аналізу даних, автоматизації задач або інтеграції з іншими системами.

## Як:

Fish Shell сам по собі не має вбудованих функцій, спеціально призначених для маніпуляції з CSV. Однак, ви можете використовувати утиліти Unix, такі як `awk`, `sed`, та `cut` для базових операцій або використовувати спеціалізовані інструменти, такі як `csvkit`, для більш складних завдань.

### Читання файлу CSV та друк першої колонки:
Використання `cut` для витягування першої колонки:
```fish
cut -d ',' -f1 data.csv
```
Приклад виводу:
```
Ім'я
Аліса
Боб
```

### Фільтрація рядків CSV на основі значення колонки:
Використання `awk` для знаходження рядків, де друга колонка відповідає "42":
```fish
awk -F, '$2 == "42" { print $0 }' data.csv
```
Приклад виводу:
```
Боб,42,Лондон
```

### Модифікація файлу CSV (наприклад, додавання колонки):
Використання `awk` для додавання колонки зі статичним значенням "НоваКолонка":
```fish
awk -F, 'BEGIN {OFS=","} {print $0,"НоваКолонка"}' data.csv > modified.csv
```
Приклад виводу у `modified.csv`:
```
Ім'я,Вік,Місто,НоваКолонка
Аліса,30,Нью-Йорк,НоваКолонка
Боб,42,Лондон,НоваКолонка
```

### Використання `csvkit` для складніших операцій:
Спочатку переконайтеся, що у вас встановлений `csvkit`. Якщо ні, встановіть його за допомогою pip: `pip install csvkit`.

**Конвертація файлу CSV у JSON:**
```fish
csvjson data.csv > data.json
```
Приклад виводу в `data.json`:
```json
[{"Ім'я":"Аліса","Вік":"30","Місто":"Нью-Йорк"},{"Ім'я":"Боб","Вік":"42","Місто":"Лондон"}]
```

**Фільтрація з використанням `csvkit` та `csvgrep`:**
```fish
csvgrep -c 2 -m 42 data.csv
```
Ця команда повторює завдання фільтрації, але використовує `csvkit`, націлюючись на колонку 2 для значення "42".

Підсумовуючи, хоча Fish Shell сам по собі може не пропонувати безпосередніх засобів для маніпуляції з файлами CSV, його безшовна інтеграція з утилітами Unix та наявність інструментів, таких як `csvkit`, забезпечують потужні можливості для роботи з файлами CSV.
