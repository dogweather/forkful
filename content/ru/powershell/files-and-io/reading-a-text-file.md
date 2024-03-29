---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:00.051213-07:00
description: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u2014 \u044D\u0442\u043E\
  \ \u0438\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u0435\u0433\u043E\
  \ \u0441\u043E\u0434\u0435\u0440\u0436\u0438\u043C\u043E\u0433\u043E \u0432 \u0444\
  \u043E\u0440\u043C\u0443, \u0441 \u043A\u043E\u0442\u043E\u0440\u043E\u0439 \u043C\
  \u043E\u0436\u0435\u0442 \u0440\u0430\u0431\u043E\u0442\u0430\u0442\u044C \u0432\
  \u0430\u0448\u0430 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0430. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\
  \u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u043E\u0431\u0440\u0430\
  \u0431\u043E\u0442\u043A\u0438 \u0434\u0430\u043D\u043D\u044B\u0445,\u2026"
lastmod: '2024-03-13T22:44:45.484061-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u2014 \u044D\u0442\u043E\
  \ \u0438\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u0435\u0433\u043E\
  \ \u0441\u043E\u0434\u0435\u0440\u0436\u0438\u043C\u043E\u0433\u043E \u0432 \u0444\
  \u043E\u0440\u043C\u0443, \u0441 \u043A\u043E\u0442\u043E\u0440\u043E\u0439 \u043C\
  \u043E\u0436\u0435\u0442 \u0440\u0430\u0431\u043E\u0442\u0430\u0442\u044C \u0432\
  \u0430\u0448\u0430 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0430. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\
  \u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u043E\u0431\u0440\u0430\
  \u0431\u043E\u0442\u043A\u0438 \u0434\u0430\u043D\u043D\u044B\u0445,\u2026"
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
---

{{< edit_this_page >}}

## Что и Почему?
Чтение текстового файла — это извлечение его содержимого в форму, с которой может работать ваша программа. Программисты делают это для обработки данных, конфигурации, ведения журналов — всякий раз, когда задействован файл в удобочитаемом формате.

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
