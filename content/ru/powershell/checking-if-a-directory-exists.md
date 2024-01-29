---
title:                "Проверка существования директории"
date:                  2024-01-28T23:55:44.931152-07:00
model:                 gpt-4-0125-preview
simple_title:         "Проверка существования директории"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Проверка на существование директории - это просто валидация наличия папки в указанном пути файловой системы. Программисты делают это для предотвращения ошибок, эффективного управления файлами и гарантии того, что данные читаются или записываются в правильные места.

## Как:

Используйте командлет `Test-Path` для проверки на существование директории. Это командлет возвращает булево значение: `$true`, если директория существует, и `$false`, если нет.

```PowerShell
# Проверка существования директории
$directoryPath = "C:\ExampleFolder"
$exists = Test-Path $directoryPath
Write-Output $exists  # Выводит True или False
```

Пример вывода:
```
True
```
или если директории не существует:
```
False
```

Вы также можете использовать его напрямую в условном операторе `if`:

```PowerShell
# Использование Test-Path в условном операторе
if (Test-Path $directoryPath) {
    Write-Output "Ага, она тут."
} else {
    Write-Output "Неа, не нахожу."
}
```

## Подробнее

Командлет `Test-Path` существует начиная с PowerShell v1.0. Это не просто утилита для одной задачи; наряду с директориями, он может использоваться для проверки файлов, ключей реестра и других объектов через различные 'пути'.

Есть альтернативы. PowerShell построен на .NET Framework, так что вы могли бы использовать методы .NET, если захотите:

```PowerShell
[system.io.directory]::Exists($directoryPath)
```

Это служит той же цели, но работает "длинным путём". Зачем заморачиваться, когда `Test-Path` создан для таких задач?

С точки зрения реализации, проверка на существование директории перед выполнением операций является лучшей практикой. Речь идет о предсказуемости. Вы ведь не станете гонять в гонках на пустом баке, верно? Так и с чтением из или записью в несуществующую директорию.

## См. также

Для получения дополнительной информации изучите следующие ссылки:

- [Документация по командлету Test-Path](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path)
- [Метод .NET Directory.Exists](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
