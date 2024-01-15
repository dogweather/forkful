---
title:                "Обчислення дати в майбутньому або минулому."
html_title:           "Fish Shell: Обчислення дати в майбутньому або минулому."
simple_title:         "Обчислення дати в майбутньому або минулому."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому
Іноді нам необхідно обчислити дату в майбутньому або минулому, наприклад, для створення резервних копій або для планування подій. Ви можете легко зробити це за допомогою команди Fish Shell "date" і деяких корисних трюків, які я поясню в цій статті.

## Як
```Fish Shell
# Отримати сьогоднішню дату
date

# Обчислити дату 5 днів назад
date -d "-5 days"

# Обчислити дату через 2 тижні
date -d "+2 weeks"

# Обчислити дату в минулому році
date -d "-1 year"

# Обчислити дату завтра
date -d "tomorrow"
```

Output:
```
Tue Sep 28 00:00:00 EDT 2021
Thu Sep 23 00:00:00 EDT 2021
Thu Oct 07 00:00:00 EDT 2021
Sat Sep 28 00:00:00 EDT 2020
Wed Sep 29 00:00:00 EDT 2021
```

## Deep Dive
Команда "date" має багато опцій для обчислення дат. Наприклад, ви можете використовувати "days", "weeks", "months" і "years" для визначення періода, а також "yesterday", "today", "tomorrow" для обчислення дати відносно поточної. Крім того, ви також можете вказати конкретну дату у форматі "month/day/year". Докладнішу інформацію про всі можливі опції ви можете знайти у документації для команди "date".

## See Also
- [Fish Shell документація для команди "date"](https://fishshell.com/docs/current/cmds/date.html)
- [Порівняння Fish Shell з іншими оболонками командного рядка](https://www.slant.co/versus/16/17/~fish_shell_vs_bash)