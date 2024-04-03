---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:29.804724-07:00
description: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0456 \u0432\u0438\u0440\
  \u0430\u0437\u0438 (regex) \u0432 Visual Basic \u0434\u043B\u044F \u0414\u043E\u0434\
  \u0430\u0442\u043A\u0456\u0432 (VBA) \u043D\u0430\u0434\u0430\u044E\u0442\u044C\
  \ \u043F\u043E\u0442\u0443\u0436\u043D\u0438\u0439 \u0441\u043F\u043E\u0441\u0456\
  \u0431 \u043F\u043E\u0448\u0443\u043A\u0443, \u0432\u0456\u0434\u043F\u043E\u0432\
  \u0456\u0434\u043D\u043E\u0441\u0442\u0456 \u0442\u0430 \u043C\u0430\u043D\u0456\
  \u043F\u0443\u043B\u044E\u0432\u0430\u043D\u043D\u044F \u0440\u044F\u0434\u043A\u0430\
  \u043C\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\
  \u2026"
lastmod: '2024-03-13T22:44:48.991647-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0456 \u0432\u0438\u0440\
  \u0430\u0437\u0438 (regex) \u0432 Visual Basic \u0434\u043B\u044F \u0414\u043E\u0434\
  \u0430\u0442\u043A\u0456\u0432 (VBA) \u043D\u0430\u0434\u0430\u044E\u0442\u044C\
  \ \u043F\u043E\u0442\u0443\u0436\u043D\u0438\u0439 \u0441\u043F\u043E\u0441\u0456\
  \u0431 \u043F\u043E\u0448\u0443\u043A\u0443, \u0432\u0456\u0434\u043F\u043E\u0432\
  \u0456\u0434\u043D\u043E\u0441\u0442\u0456 \u0442\u0430 \u043C\u0430\u043D\u0456\
  \u043F\u0443\u043B\u044E\u0432\u0430\u043D\u043D\u044F \u0440\u044F\u0434\u043A\u0430\
  \u043C\u0438."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\
  \u0456\u0432"
weight: 11
---

## Як користуватися:
Щоб використовувати регулярні вирази в VBA, спочатку потрібно увімкнути бібліотеку Microsoft VBScript Regular Expressions. У редакторі VBA перейдіть до `Інструменти` -> `Посилання`, а потім поставте галочку біля `Microsoft VBScript Regular Expressions 5.5`.

Ось базовий приклад, щоб знайти, чи існує шаблон у рядку:

```vb
Sub FindPattern()
    Dim regex As Object
    Set regex = CreateObject("VBScript.RegExp")

    With regex
        .Global = True
        .IgnoreCase = True
        .Pattern = "\bis\b"  ' Шукає слово "is"
    End With
    
    Dim testString As String
    testString = "This is a test string."
    
    If regex.Test(testString) Then
        MsgBox "Знайдено шаблон."
    Else
        MsgBox "Шаблон не знайдено."
    End If
End Sub
```

Щоб замінити шаблон у рядку:

```vb
Sub ReplacePattern()
    Dim regex As Object, replacedString As String
    Set regex = CreateObject("VBScript.RegExp")
    
    With regex
        .Global = True
        .IgnoreCase = False
        .Pattern = "\s"  ' Відповідає будь-якому символу пробілу
    End With
    
    replacedString = regex.Replace("This is a test string.", "_")
    MsgBox replacedString  ' Виводить: "This_is_a_test_string."
End Sub
```

## Поглиблений аналіз
Включення регулярних виразів до мов програмування часто походить з інструментів Unix 1970-х років. VBA інтегрував regex через бібліотеку VBScript Regular Expressions, підкреслюючи їх значення в задачах обробки тексту навіть у програмах, які зазвичай не асоціюються з інтенсивною маніпуляцією текстом, як Excel або Access.

Незважаючи на їхню потужність, regex у VBA іноді може бути менш інтуїтивним або продуктивним порівняно з більш сучасними реалізаціями в мовах, таких як Python чи JavaScript. Наприклад, модуль `re` в Python пропонує широку підтримку для іменованих груп та більш складних можливостей зіставлення шаблонів, надаючи чистіший та потенційно більш зрозумілий підхід. Однак, працюючи в екосистемі VBA, регулярні вирази залишаються незамінним інструментом для завдань, які потребують зіставлення шаблонів або маніпуляції з текстом. Часто компроміс ефективності є незначним на тлі зручності та можливостей, які regex надає при роботі з рядками в офісних додатках.
