---
title:                "Робота з csv"
html_title:           "PowerShell: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## Що & Чому?
Робота з CSV - це процес обробки табличних даних у форматі CSV (Comma-Separated Values) з використанням спеціального скрипту. CSV є одним з найпоширеніших форматів для збереження табличних даних, тому багато програмістів використовують його для роботи з даними.

## Як це зробити:
```PowerShell
# Читання даних з CSV файлу
Import-Csv C:\data.csv 

# Запис даних у CSV файл
$data | Export-Csv C:\data.csv 
```

**Вихід:**
```
Name, Age, Profession
John, 25, Developer
Jane, 30, Project Manager
```

## Глибока інформація:
1. **Історичний контекст:** CSV був розроблений в 1972 році істориком Харві Хартером в рамках свого дисертаційного дослідження. Це формат дозволяє зберігати дані у табличній формі і використовується до цього дня в різних програмах.
2. **Альтернативи:** Крім CSV, існують інші формати для збереження табличних даних, такі як JSON, XML та Excel.
3. **Деталі реалізації:** У PowerShell є вбудовані команди для роботи з CSV, які дозволяють ефективно читати, записувати та обробляти дані у цьому форматі.