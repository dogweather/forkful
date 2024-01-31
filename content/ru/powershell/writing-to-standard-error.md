---
title:                "Запись в стандартный поток ошибок"
date:                  2024-01-29T00:06:16.383961-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запись в стандартный поток ошибок"

category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/writing-to-standard-error.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Запись в стандартный поток ошибок (stderr) отправляет сообщения об ошибках и диагностику отдельно от стандартного вывода (stdout). Программисты делают это для чёткого разделения обычного вывода программы от информации об ошибках, что облегчает отладку и ведение журналов.

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
