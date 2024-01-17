---
title:                "Злиття рядків"
html_title:           "PowerShell: Злиття рядків"
simple_title:         "Злиття рядків"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що & Чому?
Конкатенація рядків - це процес з'єднання двох або більше рядків в один. Це корисна техніка для роботи зі стрічками, яку використовують програмісти для створення нових рядків або з'єднання існуючих для подальшої обробки даних.

## Як це зробити:
```PowerShell
$string1 = "Привіт"
$string2 = ", Світ!"
$string3 = $string1 + $string2
Write-Host $string3
```
Результат:
```
Привіт, Світ!
```
## Глибоке пірнання:
Конкатенація рядків використовується в багатьох мовах програмування, проте існують альтернативні методи, такі як використання оператору злиття `+=` або використання функції `join`. У PowerShell, для оптимізації продуктивності, рекомендується використовувати `StringBuilder` для з'єднання багатьох рядків. Це особливо корисно, якщо треба об'єднати велику кількість рядків.

## Дивіться також:
Дізнайтеся більше про роботу зі стрічками в PowerShell на [офіційному сайті Microsoft](https://docs.microsoft.com/en-us/powershell/scripting/learn/ps101/07-string-handling?view=powershell-7). Також можна практикуватися у використанні цієї техніки з класним [онлайн-курсом від Codecademy](https://www.codecademy.com/learn/learn-powershell).