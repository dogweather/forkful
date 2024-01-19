---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що це та навіщо це потрібно?
Порівняння двох дат - це процес, коли ми визначаємо, яка дата раніше, а яка пізніше, або вони однакові. Це необхідно для виконання завдань, як-от визначення часових проміжків, розкладів, подій, тощо.

## Як це робиться:
```PowerShell
$date1 = Get-Date -Year 2022 -Month 1 -Day 1
$date2 = Get-Date -Year 2022 -Month 2 -Day 1

if ($date1 -gt $date2) { "Перша дата більше" }
elseif ($date1 -eq $date2) { "Дати однакові" }
else { "Друга дата більше" }
```

Виводить:

```PowerShell
Друга дата більше
```

## Заглиблення

1. **Історичний контекст**: PowerShell, створений Microsoft, почав використовувати систему типів .NET з самого початку свого з'явлення в 2006 році. Так, операції над датами в PowerShell виконуються згідно із .NET типом DateTime.

2. **Альтернативи**: Лише об'єкт DateTime використовується для порівняння дат в PowerShell. Jacob McElroy написав корисний модуль на мові C#, який може бути використаний для обробки дати та часу в PowerShell, але це є виключенням.

3. **Деталі реалізації**: PowerShell використовує оператори перевірки (`-eq`, `-ne`, `-gt`, `-lt`, `-ge`, `-le`) для порівняння дат. Результатом цих операцій є булеві значення, які показують результат порівняння.

## Див. також
1. [Довідник] (https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1) по Get-Date з документації PowerShell.
2. [Пост Jacob'McElroy] (https://www.powershellmagazine.com/2014/01/24/working-with-dates-and-times-in-powershell/) про роботу з датами та часом в PowerShell.