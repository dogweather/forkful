---
title:                "Пошук та заміна тексту"
html_title:           "PowerShell: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Що і чому?

Заміна тексту - це процес заміни одного фрагмента тексту на інший. Програмісти використовують це, щоб легко і швидко змінювати декілька входжень одного фрагмента тексту на інший у своєму коді.

# Як це зробити?

```PowerShell
# Приклад 1: Заміна одного слова на інше
$text = "привіт, світ!"
$text -creplace "світ", "Україну"

# Результат: привіт, Україну!

# Приклад 2: Заміна тексту в декількох файлах
Get-ChildItem -Path "C:\код" -Filter "*.txt" | ForEach-Object { Get-Content $_.FullName | Foreach-Object { $_ -creplace "слово", "інше слово" } | Set-Content -Path $_.FullName }
```

# Що глибше?

Заміна тексту використовувалася в програмуванні ще з часів давніх комп'ютерів. В деяких мовах програмування існують спеціальні функції для цього (наприклад, в Perl), але в PowerShell використовується регулярний вираз `-creplace` для зручності. Інші альтернативи включають програми для пошуку та заміни в текстових редакторах або використання команд в терміналі.

# Дивіться також

- [Документація Microsoft про `-creplace`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7)
- [Стаття на Wikipedia про заміну тексту](https://uk.wikipedia.org/wiki/Заміна_тексту)