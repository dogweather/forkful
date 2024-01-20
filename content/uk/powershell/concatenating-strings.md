---
title:                "Конкатенація рядків"
html_title:           "PHP: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що і чому?

Об'єднання рядків - це процес з'єднання двох або більше рядків у єдиний рядок. Програмісти використовують це для організації та форматування виводу, створення динамічних запитів або команд тощо.

## Як це робиться:

```PowerShell
$str1 = "Привіт, "
$str2 = "світе!"
$concatenatedStr = $str1 + $str2
Write-Output $concatenatedStr
```
У виводі ви побачите: `Привіт, світе!`

Ви також можете використовувати функцію `-f` для об'єднання рядків:

```PowerShell
$str1 = "Привіт, {0}!"
$str2 = "світе"
$concatenatedStr = $str1 -f $str2
Write-Output $concatenatedStr
```
У виводі ви побачите: `Привіт, світе!`

## Поглиблений погляд:

Об'єднання рядків існує з самого початку програмування. В PowerShell це реалізовано за допомогою оператора `+` і параметра формату `-f`. Є інші альтернативи в PowerShell, наприклад, інтерполяція рядків:

```PowerShell
$str1 = "Привіт, "
$str2 = "світе!"
$concatenatedStr = "$str1$str2"
Write-Output $concatenatedStr
```
У виводі ви побачите: `Привіт, світе!`

## Більше інформації можна знайти тут:

- [Чудова стаття з прикладами об'єднання рядків в PowerShell](https://www.pdq.com/powershell/concatenate-strings/)
- [Об'єднання рядків в PowerShell: посібник](https://adamtheautomator.com/concatenate-strings-powershell/)
- [Microsoft Docs: Про об'єднання рядків в PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_operators?view=powershell-7.1#string-concatenation-operator-)