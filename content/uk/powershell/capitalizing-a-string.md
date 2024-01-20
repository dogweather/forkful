---
title:                "Переведення рядка в верхній регістр"
html_title:           "PowerShell: Переведення рядка в верхній регістр"
simple_title:         "Переведення рядка в верхній регістр"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що і Навіщо?

Перетворення рядка на верхній регістр — це зміна усіх символів рядка таким чином, що вони стають великими буквами. Програмісти роблять це для уніфікації даних, зручності порівняння рядків чи визначення ключових слів.

## Як зробити:

Переведемо рядок у верхній регістр у PowerShell. Подивимося на цей код:

```PowerShell
$string = "Hello, World!"
$upperCase = $string.ToUpper()
Write-Output $upperCase
```

Цей код виведе:

```PowerShell
HELLO, WORLD!
```

## Підводимо підсумки:

Переведення рядка в верхній регістр встановлює всі символи рядка у великі букви. Це полезно для того, щоб зробити порівняння рядків нечутливими до регістра, або визначити ключові слова в коді. 

У минулому, програмісти використовували цю технологію для тривіального завдання як простого порівняння рядків. 

Альтернативами є використання `ToLower()` для переведення всіх символів у нижній регістр або `Compare()` для порівняння рядків без урахування регістру. 

Ця функція реалізована в .NET, на якому базується PowerShell. 

## Дивіться також:

1. [Official Microsoft Documentation for ToUpper()](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=net-5.0)