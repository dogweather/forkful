---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:38.689136-07:00
description: "\u0421\u0447\u0438\u0442\u044B\u0432\u0430\u043D\u0438\u0435 \u0430\u0440\
  \u0433\u0443\u043C\u0435\u043D\u0442\u043E\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\
  \u043D\u043E\u0439 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 Visual Basic for\
  \ Applications (VBA) \u0441\u0432\u044F\u0437\u0430\u043D\u043E \u0441 \u0434\u043E\
  \u0441\u0442\u0443\u043F\u043E\u043C \u043A \u043F\u0430\u0440\u0430\u043C\u0435\
  \u0442\u0440\u0430\u043C, \u043F\u0435\u0440\u0435\u0434\u0430\u0432\u0430\u0435\
  \u043C\u044B\u043C \u0432 \u0432\u0430\u0448\u0443 \u043F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u043C\u0443 \u043F\u0440\u0438 \u0435\u0451\u2026"
lastmod: '2024-03-13T22:44:44.771283-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u0447\u0438\u0442\u044B\u0432\u0430\u043D\u0438\u0435 \u0430\u0440\
  \u0433\u0443\u043C\u0435\u043D\u0442\u043E\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\
  \u043D\u043E\u0439 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 Visual Basic for\
  \ Applications (VBA) \u0441\u0432\u044F\u0437\u0430\u043D\u043E \u0441 \u0434\u043E\
  \u0441\u0442\u0443\u043F\u043E\u043C \u043A \u043F\u0430\u0440\u0430\u043C\u0435\
  \u0442\u0440\u0430\u043C, \u043F\u0435\u0440\u0435\u0434\u0430\u0432\u0430\u0435\
  \u043C\u044B\u043C \u0432 \u0432\u0430\u0448\u0443 \u043F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u043C\u0443 \u043F\u0440\u0438 \u0435\u0451\u2026"
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0430\u0440\u0433\u0443\u043C\u0435\u043D\
  \u0442\u043E\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439 \u0441\
  \u0442\u0440\u043E\u043A\u0438"
---

{{< edit_this_page >}}

## Что и Почему?

Считывание аргументов командной строки в Visual Basic for Applications (VBA) связано с доступом к параметрам, передаваемым в вашу программу при её выполнении. Эта техника часто используется для влияния на поведение или результаты программы без необходимости взаимодействия с пользователем, что значительно упрощает автоматизацию и создание скриптов, делая их более универсальными.

## Как:

В отличие от более простых программных сред, VBA не имеет встроенной функции для непосредственного чтения аргументов командной строки в привычном смысле, поскольку в основном предназначен для внедрения в приложения Microsoft Office. Однако, проявив немного изобретательности, мы можем использовать Windows Script Host (WSH) или вызывать внешние API для достижения аналогичной функциональности. Вот практическое решение с использованием WSH:

1. **Создайте VBScript Для Передачи Аргументов в VBA:**

   Сначала напишите файл VBScript (*yourScript.vbs*), который запускает ваше VBA-приложение (например, макрос Excel) и передает аргументы командной строки:

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\YourMacroWorkbook.xlsm"
objExcel.Run "YourMacroName", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **Доступ к Аргументам в VBA:**

   В вашем VBA-приложении (*YourMacroWorkbook.xlsm*), измените или создайте макрос (*YourMacroName*), чтобы он принимал параметры:

```vb
Sub YourMacroName(arg1 As String, arg2 As String)
    MsgBox "Аргумент 1: " & arg1 & " Аргумент 2: " & arg2
End Sub
```

3. **Запуск Скрипта:**

   Выполните VBScript из командной строки, передавая аргументы по мере необходимости:

```shell
cscript yourScript.vbs "Привет" "Мир"
```

   Это должно привести к выполнению вашего VBA-макроса с аргументами "Привет" и "Мир", которые отображаются в сообщении.

## Глубже:

В историческом контексте VBA был разработан для расширения возможностей приложений Microsoft Office, а не как самостоятельная программная среда. Таким образом, прямое взаимодействие с командной строкой выходит за рамки его основного назначения, что объясняет отсутствие встроенной поддержки для чтения аргументов командной строки.

Указанный выше метод, хотя и эффективен, является скорее обходным решением, чем родным, использующим внешнее скриптование для преодоления пробела. Этот подход может вносить сложности и потенциальные проблемы безопасности, так как требует включения макросов и, возможно, снижения настроек безопасности для выполнения.

Для задач, сильно зависящих от аргументов командной строки или нуждающихся в более плавной интеграции с операционной системой Windows, другие языки программирования, такие как PowerShell или Python, могут предложить более надежные и безопасные решения. Эти альтернативы предоставляют прямую поддержку аргументов командной строки и лучше подходят для самостоятельных приложений или скриптов, которым требуется внешний ввод для динамического изменения их поведения.
