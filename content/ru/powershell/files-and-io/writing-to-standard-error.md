---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:16.383961-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: ."
lastmod: '2024-03-13T22:44:45.482095-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0417\u0430\u043F\u0438\u0441\u044C \u0432 \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u044B\u0439 \u043F\u043E\u0442\u043E\u043A \u043E\u0448\
  \u0438\u0431\u043E\u043A"
weight: 25
---

## Как это сделать:
```PowerShell
# Запишем простую ошибку в stderr
Write-Host "Упс, произошла ошибка!" -ForegroundColor Red 1>&2

# Запишем ошибку, используя командлет Write-Error
Write-Error "Это сообщение об ошибке!"

# Использование $ErrorView для отображения или обработки ошибок по-разному
$ErrorView = "CategoryView"
try {
    Get-ChildItem "nonexistentfile.txt"
} catch {
    Write-Host $_.Exception.Message -ForegroundColor Red 1>&2
}
```

Пример вывода:
```
Упс, произошла ошибка!
Write-Error: Это сообщение об ошибке!
Get-ChildItem: Не удается найти путь 'C:\...\nonexistentfile.txt', потому что он не существует.
```

## Погружение в детали
Исторически, разделение stdout и stderr имеет корни Unix, позволяя пользователям перенаправлять выводы отдельно. PowerShell, унаследовавший эту концепцию, использует Write-Error и Write-Host (с перенаправлением), среди прочих командлетов, для отправки сообщений в stderr. Под капотом PowerShell использует методы .NET для реализации этой возможности.

Альтернативы включают использование операторов throw или блоков обработки исключений; однако, они влияют на поток скрипта. Запись в stderr не прерывает выполнение, если вы специально не проверите переменную $Error или не используете параметры -ErrorAction.

## Смотрите также
- [about_Redirection](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_redirection)
- [Write-Error](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/write-error)
- [about_Try_Catch_Finally](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-exceptions?view=powershell-7.1)
