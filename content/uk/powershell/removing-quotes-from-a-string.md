---
title:                "Видалення лапок зі строки"
date:                  2024-01-26T03:42:51.733061-07:00
model:                 gpt-4-0125-preview
simple_title:         "Видалення лапок зі строки"

category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Видалення лапок з рядка в PowerShell видаляє одинарні (`'`) або подвійні (`"`) лапки, якими обрамлено ваш текст. Програмісти часто потребують очищення рядків для обробки, порівняння або виводу, особливо при роботі з введенням користувача або парсингом файлів.

## Як це зробити:
Ви можете використовувати оператор `-replace` для видалення лапок з рядка. Ось як:

```PowerShell
# Замініть одинарні лапки
$stringWithSingleQuotes = "'Привіт, Світ!'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # Вивід: Привіт, Світ!

# Замініть подвійні лапки
$stringWithDoubleQuotes = '"Привіт, Світ!"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # Вивід: Привіт, Світ!
```

Для обох типів:

```PowerShell
$stringWithQuotes = '"Привіт," сказала вона.'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # Зверніть увагу на використання класу символів регулярних виразів
Write-Output $cleanString  # Вивід: Привіт, сказала вона.
```

Приклад виводу в консолі буде виглядати приблизно так:

```
Привіт, Світ!
Привіт, Світ!
Привіт, сказала вона.
```

## Поглиблений розгляд
Давним-давно, до того як PowerShell з’явився на світло у Microsoft, обробка тексту в Windows часто була доменом пакетних скриптів, які мали обмежені можливості. Поява PowerShell принесла з собою потужні функції маніпуляції з рядками, які зробили скриптинг значно міцнішим.

Існують альтернативи `-replace`, такі як використання методу `.Trim()` для видалення лапок лише на початку і в кінці рядка, але вони не пропонують такого ж контролю або підтримки регулярних виразів.

```PowerShell
# Використання .Trim() для лапок на початку і в кінці
$stringWithQuotes = '"Привіт, Світ!"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # Вивід: Привіт, Світ!
```

Варто зазначити, що `-replace` використовує регулярні вирази за лаштунками, тому коли ви працюєте з ним, майте на увазі, що спеціальні символи потребують екранування, якщо ви хочете їх визначити. Якщо вам потрібен більш детальний контроль над видаленням лапок, занурення у регулярні вирази з використанням `-replace` є правильним шляхом, надаючи вам велику гнучкість.

## Також дивіться
- Для більшої інформації про регулярні вирази в PowerShell, перегляньте офіційні документи: [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- Відкрийте для себе інші методи рядків: [Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/en-us/dotnet/api/system.string.trim?view=net-6.0)
