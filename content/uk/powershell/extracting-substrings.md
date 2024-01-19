---
title:                "Видобування підрядків"
html_title:           "PowerShell: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що та чому?

Вилучення підрядків є процесом, коли з заданого рядка витягуються певні частини тексту за певними критеріями. Програмісти зазвичай роблять це, щоб отримати конкретну інформацію з текстових даних, або для форматування даних у бажаний вигляд.

## Як зробити:

```PowerShell
# Приклад 1:
$підрядок = "Це прикладний текст"
$вилучені_підрядки = $підрядок.Substring(8, 8)
Write-Host "Вилучені підрядки: $вилучені_підрядки"
# Вивід: "приклад"

# Приклад 2:
$підрядок = "Ім`я-Прізвище-Вік"
$дані = $підрядок.Split("-")
Write-Host "Ім`я: $($дані[0])"
Write-Host "Прізвище: $($дані[1])"
Write-Host "Вік: $($дані[2])"
# Вивід: 
# Ім'я: Ім`я
# Прізвище: Прізвище
# Вік: Вік
```

## Глибокий занурення:

Вилучення підрядків користувалося популярністю в ранніх версіях мов програмування, коли можливість простоти роботи з текстом була обмежена. Зараз є інші способи роботи з текстом, такі як регулярні вирази, які можуть бути більш потужними та гнучкими в роботі з рядками. У PowerShell є кілька вбудованих функцій для вилучення підрядків, таких як `Substring()` та `Split()`, але також є можливість використання регулярних виразів для більш складних завдань.

## Дивіться також:

- [Документація Microsoft про вилучення підрядків в PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_substr)
- [Засіб Regex для роботи з регулярними виразами в PowerShell](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netcore-3.1)