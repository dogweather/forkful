---
title:                "Пошук та заміна тексту"
date:                  2024-01-20T17:58:43.130705-07:00
model:                 gpt-4-1106-preview
simple_title:         "Пошук та заміна тексту"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? / Що це таке і навіщо?
Пошук та заміна тексту - це процес виявлення конкретних фрагментів тексту та їх подальшої модифікації чи заміни на інший текст. Програмісти використовують це для автоматизації редагування коду, управління налаштуваннями, або масової зміни файлів.

## How to: / Як це зробити:
Давайте подивимося на базовий приклад: ви хочете знайти слово "hello" у файлі й замінити його на "hi". 

```PowerShell
# Замінити 'hello' на 'hi' в конкретному файлі:
(Get-Content -path "C:\your\file.txt") -replace 'hello', 'hi' | Set-Content -path "C:\your\file.txt"
```

Виведемо результат на екран та замінимо у виводі:

```PowerShell
# Виводимо вміст файлу, заміняємо 'hello' на 'hi' і друкуємо на екрані
Get-Content -path "C:\your\file.txt" | ForEach-Object { $_ -replace 'hello', 'hi' }
```

## Deep Dive / Поглиблений розгляд:
Перші скрипти для пошуку та заміни з'явилися ще в ранніх текстових редакторах. Щодо PowerShell, `-replace` працює з регулярними виразами (regex), таким чином даючи екстремальну гнучкість і силу. Існують також і альтернативні методи для заміни тексту, наприклад `sed` у Linux або текстові редактори як Notepad++, які мають GUI для цього процесу. Щодо впровадження, команда `-replace` в PowerShell може замінювати текст у потоках даних, що робить її ідеальною для пайплайнів і автоматизації.

## See Also / Дивіться також:
- [Офіційна документація PowerShell `-replace` Operator](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators)
- [Notepad++](https://notepad-plus-plus.org/)
- [SED manual](https://www.gnu.org/software/sed/manual/sed.html)