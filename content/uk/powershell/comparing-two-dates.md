---
title:                "Порівняння двох дат"
html_title:           "PowerShell: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Що це і для чого?
Порівняння двох дат - це процес визначення різниці між двома датами. Це важлива операція для програмістів, яка дозволяє досліджувати і розуміти зміни у часі.

# Як це зробити:
```PowerShell
$date1 = Get-Date "01/01/2020"
$date2 = Get-Date "05/01/2020"

$diff = New-TimeSpan $date1 $date2
# результатом буде різниця в часі між двома датами
```

# Глибше вдивимося:
Порівняння дат є важливою складовою процесу в розробці програмного забезпечення. Його можна використовувати для перевірки того, чи наступило певне подія у встановлені строки часу. Існують інші альтернативні методи порівняння дат, такі як використання команди `Compare-Object` або використання умовних операторів `if...else`.

# Дивіться також:
Ви можете ознайомитися з нашим матеріалом про інші використання PowerShell для роботи з датами і часом:
1. https://docs.microsoft.com/en-us/powershell/scripting/samples/reference/timestamp/timestamp?view=powershell-7.1 
2. https://4sysops.com/archives/use-powershell-to-convert-date-format 

Будьте творчими та експериментуйте з іншими командами та методами, щоб знайти найбільш зручний для вас спосіб порівняння дат у PowerShell. Це допоможе вам в розробці більш ефективних та точних програм.