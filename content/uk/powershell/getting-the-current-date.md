---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:16:30.982528-07:00
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"

category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Отримання поточної дати — це процес визначення дати та часу в даний момент. Програмісти використовують це для логування, штампів часу, або функцій, що залежать від часу.

## How to: (Як це зробити:)
Отримайте поточну дату та час так просто:

```PowerShell
# Отримати поточну дату і час
Get-Date

# Форматувати вивід
Get-Date -Format "yyyy-MM-dd HH:mm:ss"
```

Зразок виводу:

```PowerShell
Середа, 20 квітня 2023 14:55:12

2023-04-20 14:55:12
```

## Deep Dive (Поглиблений розбір)
`Get-Date` у PowerShell - стандартна команда для отримання дати. Вона відображає поточну дату та час вашої системи, використовуючи .NET клас `DateTime`. В історичному контексті, команди такого типу завжди були ключовими в більшості мов програмування, оскільки працювати з датами та часом є фундаментальним аспектом багатьох додатків.

Альтернативами можуть бути `$env:COMPUTERNAME` у PowerShell для отримання ім'я комп'ютера або `[System.DateTime]::Now` для отримання поточного часу через .NET напряму.

Деталі імплементації включають роботу вакуумного часового пояса (UTC) з використанням `Get-Date`. Ось як:

```PowerShell
Get-Date -Format "o" # ISO 8601 format with UTC timezone
```

## See Also (Дивіться також)
- [Офіційна документація `Get-Date`](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date)
- [.NET клас DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
