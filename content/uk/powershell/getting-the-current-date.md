---
title:                "Отримання поточної дати"
html_title:           "PowerShell: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

Що & Чому?

Дізнавання поточної дати - це процес отримання поточної дати та часу на комп'ютері. Це важливо для програмістів, оскільки ця інформація може бути використана для відстеження часу створення чи зміни файлів, а також для синхронізації даних між різними пристроями.

Як це зробити:

```PowerShell
# Отримати поточну дату
Get-Date

# Вивести поточну дату у визначеному форматі
Get-Date -Format "dd-MM-yyyy"

# Отримати поточний час
Get-Date -Format "HH:mm:ss"
```

Вивід:

```
Вівторок, 20 листопада 2018 01:30:25
20-11-2018
01:30:25
```

Глибше:

Отримання поточної дати є необхідною операцією в багатьох програмах та скриптах. Цей процес став доступним завдяки введенню різних конкретних функцій для роботи з датами в сучасних мовах програмування, таких як PowerShell. На сьогоднішній день існує багато альтернативних методів отримання поточної дати, але саме PowerShell є потужним інструментом для роботи з датами та часом.

Подивіться також:

- [Офіційна документація PowerShell для Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-6)
- [Блог IT-SCHOOL по роботі з датами в PowerShell](http://it-school.in.ua/unix/dateTime.htm)