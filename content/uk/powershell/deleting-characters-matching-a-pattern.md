---
title:                "Видалення символів, що відповідають патерну"
html_title:           "C: Видалення символів, що відповідають патерну"
simple_title:         "Видалення символів, що відповідають патерну"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Видалення символів за шаблоном у PowerShell

## Що Навіщо?

Видалення символів за шаблоном - це процес, коли програміст видаляє або замінює символи у рядках, що відповідають певному шаблону. Використовують це, щоб виправити помилки у даних або привести текст до бажаного формату.

## Як це робиться:

За допомогою cmdlet `Replace` в PowerShell можна видалити символи, що співпадають з шаблоном.

```PowerShell
PS C:\> $str = "Hello, World!";
PS C:\> $str -Replace '[,!]',''
```

У вихідному результаті кома та знак оклику буде видалено:

```PowerShell
PS C:\> Hello World
```

## Поглиблено:

При видаленні символів за шаблоном важливо знати, що PowerShell використовує регулярні вирази (regex) для шаблону. Регулярні вирази були розроблені в 1956 році щоб маніпулювати текстом.

Є методи-альтернативи, такі як `Substring` або `Remove`, але `Replace` є найбільш гнучким.

## Див. також:

- [Основи регулярних виразів у PowerShell](https://devblogs.microsoft.com/scripting/powertip-use-powershell-to-replace-characters-in-string/)
- [Офіційна документація PowerShell про Replace](https://docs.microsoft.com/uk-ua/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1#replacement-operator) 

*Примітка: в даному матеріалі використовується найновіша версія PowerShell на момент написання.*