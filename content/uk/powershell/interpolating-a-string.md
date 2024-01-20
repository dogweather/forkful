---
title:                "Інтерполяція рядка"
html_title:           "Java: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Інтерполяція рядків - це процес внесення змінних або виразів безпосередньо в рядок. Програмісти це роблять для створення динамічних рядків, що зручно під час форматування або виводу даних.

## Як це зробити:
Отже, як це працює в PowerShell? Дуже просто. Ось приклад коду і виводу даних:

```PowerShell
$planet = "Земля"
$text = "Ми живемо на планеті $planet."
Write-Host $text
```

Цей код виведе: "Ми живемо на планеті Земля."

## Глибше занурення
Інтерполяція рядків є дуже важливою властивістю мов програмування, яка була внесена відразу в багатьох мовах як Perl і PHP. В PowerShell інтерполяція рядків використовується з початкової версії. Хоча є альтернативи, такі як з'єднання рядків або форматування рядків, інтерполяція рядків є найбільш простим і безшовним способом включення змінних в рядки.

## Дивіться також
Подальше читання і навчальні матеріали:

- Офіційна документація PowerShell з примерами інтерполяції рядків: [Тут](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules)

- Чудова стаття по інтерполяції рядків в PowerShell: [Тут](https://ss64.com/ps/syntax-f-operator.html)
