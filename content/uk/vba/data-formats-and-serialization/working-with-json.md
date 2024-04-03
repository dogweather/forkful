---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:06.776383-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : VBA \u0437\u0430 \u0437\u0430\u043C\u043E\u0432\u0447\u0443\u0432\u0430\u043D\u043D\
  \u044F\u043C \u043D\u0435 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u0443\u0454\
  \ \u0430\u043D\u0430\u043B\u0456\u0437 JSON \u0430\u0431\u043E \u0439\u043E\u0433\
  \u043E \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F, \u0442\u043E\u043C\
  \u0443 \u043C\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u0454\
  \u043C\u043E \u0441\u043A\u0440\u0438\u043F\u0442\u043E\u0432\u0443 \u043C\u043E\
  \u0432\u0443, \u044F\u043A-\u043E\u0442 JScript (\u0447\u0435\u0440\u0435\u0437\
  \ \u043E\u0431'\u0454\u043A\u0442\u2026"
lastmod: '2024-03-13T22:44:49.046620-06:00'
model: gpt-4-0125-preview
summary: "VBA \u0437\u0430 \u0437\u0430\u043C\u043E\u0432\u0447\u0443\u0432\u0430\u043D\
  \u043D\u044F\u043C \u043D\u0435 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u0443\
  \u0454 \u0430\u043D\u0430\u043B\u0456\u0437 JSON \u0430\u0431\u043E \u0439\u043E\
  \u0433\u043E \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F, \u0442\u043E\
  \u043C\u0443 \u043C\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\
  \u0454\u043C\u043E \u0441\u043A\u0440\u0438\u043F\u0442\u043E\u0432\u0443 \u043C\
  \u043E\u0432\u0443, \u044F\u043A-\u043E\u0442 JScript (\u0447\u0435\u0440\u0435\u0437\
  \ \u043E\u0431'\u0454\u043A\u0442 ScriptControl), \u0434\u043B\u044F \u0430\u043D\
  \u0430\u043B\u0456\u0437\u0443 \u0440\u044F\u0434\u043A\u0456\u0432 JSON \u0456\
  \ \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u043E\u0431'\u0454\u043A\
  \u0442\u0456\u0432 JSON."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

## Як це зробити:
VBA за замовчуванням не підтримує аналіз JSON або його створення, тому ми використаємо скриптову мову, як-от JScript (через об'єкт ScriptControl), для аналізу рядків JSON і створення об'єктів JSON. Ось як ви можете аналізувати рядок JSON у VBA:

```basic
Function ParseJSON(ByVal jsonString As String) As Object
    Dim scriptControl As Object
    Set scriptControl = CreateObject("MSScriptControl.ScriptControl")
    scriptControl.Language = "JScript"
    
    scriptControl.Eval "var obj = (" & jsonString & ")"
    Set ParseJSON = scriptControl.CodeObject.obj
End Function

Sub DemoParseJSON()
    Dim jsonString As String
    jsonString = "{""name"":""John"", ""age"":30, ""city"":""New York""}"
    
    Dim parsed As Object
    Set parsed = ParseJSON(jsonString)
    
    MsgBox "Ім'я: " & parsed.name & ", Вік: " & parsed.age & ", Місто: " & parsed.city
End Sub
```

Щоб створити JSON, ви могли б використати подібний підхід, будуючи рядок JSON через конкатенацію:

```basic
Function GenerateJSON(name As String, age As Integer, city As String) As String
    GenerateJSON = "{""name"":""" & name & """, ""age"":" & age & ", ""city"":""" & city & """}"
End Function

Sub DemoGenerateJSON()
    Dim jsonString As String
    jsonString = GenerateJSON("Jane", 28, "Los Angeles")
    
    MsgBox jsonString
End Sub
```

## Поглиблений Розгляд
Показані підходи використовують ScriptControl для роботи з JSON, по суті, передаючи роботу на двигун JavaScript. Це творчий обхідний шлях, але не обов'язково найефективніший або найсучасніший спосіб працювати з JSON у контексті VBA. У більш складних додатках цей метод може стати громіздким і вводити додаткові навантаження на продуктивність або питання безпеки, оскільки ScriptControl виконується в середовищі, яке має повний доступ до комп'ютера-хоста.

Інші середовища програмування, такі як Python або JavaScript, пропонують вбудовану підтримку для JSON, роблячи їх більш підходящими для додатків, які вимагають широкомасштабної маніпуляції з JSON. Ці мови надають комплексні бібліотеки, які полегшують не тільки аналіз і генерацію, але й запитування і форматування даних JSON.

Незважаючи на ці обмеження у VBA, розуміння того, як працювати з JSON, є життєво важливим у світі, де обмін даними через Інтернет і конфігураційні файли переважно форматовані у JSON. Для програмістів VBA освоєння цих технік відкриває можливості для інтеграції з веб-API, інтерпретації конфігураційних файлів або навіть створення простих веб-додатків. Однак, коли проекти ростуть у складності або вимагають високої продуктивності, розробники можуть розглянути можливість використання більш підходящих для роботи з JSON середовищ програмування.
