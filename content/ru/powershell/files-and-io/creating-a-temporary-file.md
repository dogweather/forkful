---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:06.893555-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0427\u0442\u043E\u0431\u044B \u0441\u043E\u0437\u0434\u0430\u0442\
  \u044C \u0432\u0440\u0435\u043C\u0435\u043D\u043D\u044B\u0439 \u0444\u0430\u0439\
  \u043B \u0432 PowerShell, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\
  \u0442\u0435 `New-TemporaryFile`. \u042D\u0442\u0430 \u043A\u043E\u043C\u0430\u043D\
  \u0434\u043B\u0435\u0442\u0430 \u0441\u043E\u0437\u0434\u0430\u0435\u0442 \u0432\
  \u0440\u0435\u043C\u0435\u043D\u043D\u044B\u0439 \u0444\u0430\u0439\u043B \u0432\
  \ \u0432\u0430\u0448\u0435\u0439 \u0432\u0440\u0435\u043C\u0435\u043D\u043D\u043E\
  \u0439\u2026"
lastmod: '2024-03-13T22:44:45.487928-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u0441\u043E\u0437\u0434\u0430\u0442\u044C\
  \ \u0432\u0440\u0435\u043C\u0435\u043D\u043D\u044B\u0439 \u0444\u0430\u0439\u043B\
  \ \u0432 PowerShell, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\
  \u0435 `New-TemporaryFile`."
title: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0432\u0440\u0435\u043C\u0435\
  \u043D\u043D\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 21
---

## Как это сделать:
Чтобы создать временный файл в PowerShell, используйте `New-TemporaryFile`. Эта командлета создает временный файл в вашей временной папке. Вот это волшебное заклинание:

```PowerShell
$tempFile = New-TemporaryFile
```

Эта строка вызывает совершенно новый временный файл из цифрового эфира. Хотите знать, где он находится? Просто введите:

```PowerShell
$tempFile.FullName
```

И бац! Он скажет вам путь к файлу. Когда вы закончили и хотите очистить, просто удалите его:

```PowerShell
Remove-Item $tempFile.FullName
```

Файл исчезает, не оставляя следов.

## Погружение в детали
Теперь давайте заглянем под капот. Исторически временные файлы использовались с самого начала компьютерной эры, главным образом потому, что оперативная память была дефицитной и дорогой. Эти переходные файлы были решением ограниченной памяти.

Что касается альтернатив, некоторые разработчики самостоятельно создают пути к своим временным файлам с помощью `[System.IO.Path]::GetTempFileName()`, которая работает в разных языках, поддерживаемых .NET, и дает больше контроля.

В PowerShell `New-TemporaryFile` на самом деле является элегантной оболочкой вокруг этого метода .NET. Он создает файл по пути вида `C:\Users\ВашеИмя\AppData\Local\Temp\tmpXXXX.tmp` (`XXXX` - это случайное число). Расширение `.tmp` является конвенцией, сигнализирующей о временном характере.

Помните, временные файлы следует утилизировать должным образом. Если вы создаете их много или обрабатываете конфиденциальные данные, вы должны удалять их безвозвратно, чтобы предотвратить утечку данных.

## Смотрите также
- Для получения дополнительной информации о `New-TemporaryFile` смотрите [документацию](https://docs.microsoft.com/ru-ru/powershell/module/microsoft.powershell.utility/new-temporaryfile).
- Изучите методы класса `System.IO.Path` на [Microsoft Docs](https://docs.microsoft.com/ru-ru/dotnet/api/system.io.path?view=net-6.0).
