---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Отримання поточної дати в PowerShell - це процес, который дозволяє вам знати який сьогодні день. Програмісти використовують це для відстеження записів, для роботи із файлами та базами даних і для аналізу часових рамок.

## Як зробити:

Otримати поточну дату в PowerShell можна за допомогою команди `Get-Date`. Ось приклад:

```PowerShell
Get-Date
```

Видача прикладу:

```PowerShell
Tuesday, October 26, 2021 12:14:16 PM
```

## Глибше занурення

В PowerShell, `Get-Date` був одним із перших cmdlet для роботи із датою й часом, що виходить із історичного контексту, коли навіть найпростіші операції часто потребують помітних зусиль.
 
Є альтернативи `Get-Date` в PowerShell. Наприклад, ви можете використовувати `[datetime]::Now` для отримання поточного часу i дати, але `Get-Date` набагато гнучкіше, оскільки воно дозволяє встановити специфічний формат дати при необхідності.

`Get-Date` використовує API Windows для взаємодії з системним часом і датою, а отже, він працює єднаково на всіх платформах, де виконується PowerShell.

## Дивись також

Для більш детального ознайомлення з роботою з датою і часом в PowerShell спробуйте подивитися наступні посилання:

- Офіційна документація PowerShell `Get-Date`: https://docs.microsoft.com/uk-ua/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1
- Проект GitHub #PowerShell для додаткових прикладів і обговорень: https://github.com/PowerShell/PowerShell.
- "Working with Dates and Times in PowerShell" - блог, який глибше вивчає цю тему: https://devblogs.microsoft.com/scripting/