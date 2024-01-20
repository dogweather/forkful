---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Elixir: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і чому?

Перетворення рядка в нижній регістр — це процес приведення всіх символів рядка до нижнього регістру. Програмісти використовують цей прийом для нормалізації даних, задля подальшого порівняння чи обробки.

## Як це зробити:

PowerShell пропонує простий спосіб перетворення рядка в нижній регістр з використанням методу `.ToLower()`. Ось приклад коду і його виводу:

```PowerShell
$str = "POWERShell"
$lower = $str.ToLower()
Write-Output $lower
```

Цей код поверне вам:

```PowerShell
powershell
```

## Підводимо підсумки:

Ідея перетворення рядків в нижній регістр вперше з'явилася в мовах програмування ще у 70-х роках 20-го століття. Сучасні версії його також реалізують, але з вдосконаленнями та оптимізаціями.

Альтернативною опцією для `.ToLower()` є використання `.ToUpper()` для приведення рядка до верхнього регістру. Все залежить від вас та ваших потреб.

Поводовчин перекладу рядка в нижнє розташування в PowerShell – у тому, що він шукає Unicode-версії для кожного символа у верхньому регістрі та замінює їх на відповідний символ нижнього регістру.

## Дивіться також:

[Powershell String Methods](https://www.powershelltutorial.net/powershell-string-methods/): Усе, що вам потрібно знати про методи рядка в PowerShell.
[Understanding the PowerShell String ‘.toLower()’ Method](https://www.dummies.com/programming/programming-microsoft-powershell-3-0-part-2/understanding-the-powershell-string-tolower-method/): Глибоке розуміння методу `.toLower()` у PowerShell.