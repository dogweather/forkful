---
title:                "Обчислення дати у майбутньому або минулому"
date:                  2024-01-20T17:32:20.542708-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Розрахунок дати в майбутньому чи минулому — це процес знаходження дат після чи перед встановленою точкою в часі. Програмісти виконують це для планування подій, перевірки термінів дії або для функціонування таймерів та розкладів.

## Як це зробити:
Нижче наведено приклади коду та результати їх виконання.

```PowerShell
# Додати 10 днів до сьогоднішньої дати
$futureDate = (Get-Date).AddDays(10)
Write-Output "Десять днів від сьогодні: $futureDate"

# Відняти 20 днів від сьогоднішньої дати
$pastDate = (Get-Date).AddDays(-20)
Write-Output "Двадцять днів тому: $pastDate"

# Відобразити дату та час через 2 роки, 1 місяць та 3 дні
$specificFuture = (Get-Date).AddYears(2).AddMonths(1).AddDays(3)
Write-Output "Через 2 роки, 1 місяць і 3 дні буде: $specificFuture"
```

```output
Десять днів від сьогодні: <майбутня дата>
Двадцять днів тому: <минула дата>
Через 2 роки, 1 місяць і 3 дні буде: <майбутня дата>
```

## Поглиблений огляд:
Розрахунок дати був завжди актуальний, від календарів на стінах до цифрових нагадувань. У PowerShell, `Get-Date` є основною командою для роботи з часом. Методи `AddDays()`, `AddMonths()` та інші модифікують дати легко і інтуїтивно.

Альтернативи? Ще можна використовувати `[datetime]` клас з .NET, але `Get-Date` простіше. Щодо історичного контексту, час має багату історію комп'ютеризації, з Y2K як найбільший приклад. Імплементаційні деталі? Зверніть увагу на часові зони і переведення часу, коли обраховуєте дати.

## Дивіться також:
- Офіційна документація по `Get-Date`: https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date
- .NET `DateTime` клас: https://docs.microsoft.com/en-us/dotnet/api/system.datetime
- Розуміння часових зон: https://docs.microsoft.com/en-us/windows-hardware/manufacture/desktop/default-time-zones
