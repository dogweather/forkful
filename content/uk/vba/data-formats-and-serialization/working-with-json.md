---
title:                "Робота з JSON"
aliases:
- /uk/vba/working-with-json/
date:                  2024-02-01T22:07:06.776383-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/vba/working-with-json.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?

JSON (JavaScript Object Notation) — це легкий формат обміну даними, який легко читається і пишеться людьми, а також легко аналізується і генерується машинами. Програмісти використовують JSON для передачі даних між сервером і веб-додатком або для зберігання інформації в структурованому, доступному вигляді у різноманітних середовищах програмування, включаючи Visual Basic for Applications (VBA).

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
