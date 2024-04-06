---
date: 2024-01-20 17:40:53.150191-07:00
description: "How to: | \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438: \u0406\u0434\u0435\u044F \u0442\u0438\u043C\u0447\u0430\u0441\u043E\u0432\
  \u0438\u0445 \u0444\u0430\u0439\u043B\u0456\u0432 \u0456\u0441\u043D\u0443\u0454\
  \ \u0434\u0443\u0436\u0435 \u0434\u0430\u0432\u043D\u043E \u0456 \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F \u043D\
  \u0435 \u043B\u0438\u0448\u0435 \u0432 Windows, \u0430\u043B\u0435 \u0439 \u0432\
  \ UNIX-\u043F\u043E\u0434\u0456\u0431\u043D\u0438\u0445 \u0441\u0438\u0441\u0442\
  \u0435\u043C\u0430\u0445. Alternatives to\u2026"
lastmod: '2024-04-05T22:51:02.691550-06:00'
model: gpt-4-1106-preview
summary: "| \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:\
  \ \u0406\u0434\u0435\u044F \u0442\u0438\u043C\u0447\u0430\u0441\u043E\u0432\u0438\
  \u0445 \u0444\u0430\u0439\u043B\u0456\u0432 \u0456\u0441\u043D\u0443\u0454 \u0434\
  \u0443\u0436\u0435 \u0434\u0430\u0432\u043D\u043E \u0456 \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F \u043D\u0435\
  \ \u043B\u0438\u0448\u0435 \u0432 Windows, \u0430\u043B\u0435 \u0439 \u0432 UNIX-\u043F\
  \u043E\u0434\u0456\u0431\u043D\u0438\u0445 \u0441\u0438\u0441\u0442\u0435\u043C\u0430\
  \u0445. Alternatives to using `$tempFile` in PowerShell include creating a Random\
  \ file name with `[System.IO.Path]::GetRandomFileName()` that doesn't automatically\
  \ create the file, or using a custom function for even greater control. When creating\
  \ a temporary file, it's critical to ensure cleanup to prevent temporary files from\
  \ consuming disk space. Ideally, temporary files are removed after their intended\
  \ use, as shown in the example above. PowerShell's approach combines .NET class\
  \ methods with PowerShell cmdlets, showing its interoperable nature. This hybrid\
  \ style is common in PowerShell, making it versatile for various tasks."
title: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\u043C\u0447\
  \u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 21
---

## How to: | Як це зробити:
```PowerShell
# Створення тимчасового файла
$tempFile = [System.IO.Path]::GetTempFileName()

# Робота з тимчасовим файлом
Set-Content -Path $tempFile -Value 'Some temporary data.'
Get-Content -Path $tempFile

# Виведення прикладу результату
"Some temporary data."

# Видалення тимчасового файла
Remove-Item $tempFile

# Перевірка, чи файл більше не існує
Test-Path $tempFile

# Виведення прикладу результату
False
```

## Deep Dive | Поглиблено:
Ідея тимчасових файлів існує дуже давно і використовується не лише в Windows, але й в UNIX-подібних системах. Alternatives to using `$tempFile` in PowerShell include creating a Random file name with `[System.IO.Path]::GetRandomFileName()` that doesn't automatically create the file, or using a custom function for even greater control.

When creating a temporary file, it's critical to ensure cleanup to prevent temporary files from consuming disk space. Ideally, temporary files are removed after their intended use, as shown in the example above.

PowerShell's approach combines .NET class methods with PowerShell cmdlets, showing its interoperable nature. This hybrid style is common in PowerShell, making it versatile for various tasks.

## See Also | Додатково:
- Official PowerShell Documentation: https://docs.microsoft.com/en-us/powershell/
- .NET System.IO Namespace: https://docs.microsoft.com/en-us/dotnet/api/system.io?view=net-5.0
- About Temporary Files in Windows: https://support.microsoft.com/en-us/topic/description-of-the-use-of-temporary-files-in-windows-7cf55d59-ba71-57cf-ba45-3a8d74f1b6cb
