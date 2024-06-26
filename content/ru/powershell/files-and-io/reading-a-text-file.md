---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:00.051213-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043D\u0430\u0447\u043D\
  \u0435\u043C \u0441 \u043E\u0441\u043D\u043E\u0432! \u0412\u043E\u0442 \u043A\u0430\
  \u043A \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0447\u0438\u0442\u0430\
  \u0442\u044C \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\u0439 \u0444\u0430\
  \u0439\u043B \u0432 PowerShell."
lastmod: '2024-04-05T22:38:44.694720-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043D\u0430\u0447\u043D\u0435\
  \u043C \u0441 \u043E\u0441\u043D\u043E\u0432!."
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 22
---

## Как это сделать:
Давайте начнем с основ! Вот как вы можете читать текстовый файл в PowerShell:

```PowerShell
# Получение содержимого файла
$content = Get-Content -Path "C:\путь\к\вашему\файлу.txt"
# Вывод содержимого в консоль
Write-Output $content
```

Пример вывода может выглядеть так, если ваш файл содержит несколько строк текста:
```
Привет, PowerShell!
Конец файла.
```

Теперь хотите читать построчно?

```PowerShell
# Чтение файла построчно
$lines = Get-Content -Path "C:\путь\к\вашему\файлу.txt" -ReadCount 0
foreach ($line in $lines) {
    Write-Output $line
}
```

Тот же пример вывода, что и выше, но обрабатывается по одной строке за раз.

## Подробный Разбор
Задолго до PowerShell, командные инструменты вроде `cat` в системах, подобных UNIX, или `type` в DOS, были выбором для чтения файлов. `Get-Content` в PowerShell — это сегодняшний острый инструмент для этого, с дополнительными преимуществами, такими как чтение построчно, что помогает избежать перегрузки памяти огромными файлами.

Помимо `Get-Content`, у нас есть классы `.NET` для большего контроля — встречайте `System.IO.StreamReader`:

```PowerShell
$stream = [System.IO.StreamReader] "C:\путь\к\вашему\файлу.txt"
try {
    while ($line = $stream.ReadLine()) {
        Write-Output $line
    }
}
finally {
    $stream.Close()
}
```

Это более эффективный с точки зрения памяти метод, полезный для огромных текстовых массивов.

Альтернативы? Ну, вы могли бы использовать `Import-Csv` для файлов CSV или `ConvertFrom-Json` для JSON, если хотите перенести данные в структурированные объекты. Но придерживайтесь `Get-Content` для работы с сырым текстом.

## Смотрите также
Ознакомьтесь с официальной документацией для обнаружения больших возможностей:

- [Документация по Get-Content](https://docs.microsoft.com/ru-ru/powershell/module/microsoft.powershell.management/get-content)
- [О автоматических переменных](https://docs.microsoft.com/ru-ru/powershell/module/microsoft.powershell.core/about/about_automatic_variables) - Это дает представление о переменных вроде `$_`, которые могут быть полезны для обработки встроенного кода.
- [Использование возможностей .NET в PowerShell](https://docs.microsoft.com/ru-ru/powershell/scripting/developer/hosting/adding-and-invoking-commands?view=powershell-7.1) - Для тех, кто глубже погружается в фреймворк .NET в рамках PowerShell.
