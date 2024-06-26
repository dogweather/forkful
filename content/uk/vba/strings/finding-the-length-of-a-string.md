---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:59.119168-07:00
description: "\u042F\u043A: \u0423 VBA \u0444\u0443\u043D\u043A\u0446\u0456\u044F\
  \ `Len` \u0454 \u0432\u0430\u0448\u0438\u043C \u0433\u043E\u043B\u043E\u0432\u043D\
  \u0438\u043C \u0456\u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u043E\u043C\
  \ \u0434\u043B\u044F \u0437\u043D\u0430\u0445\u043E\u0434\u0436\u0435\u043D\u043D\
  \u044F \u0434\u043E\u0432\u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430\
  . \u0412\u043E\u043D\u0430 \u043F\u043E\u0432\u0435\u0440\u0442\u0430\u0454 \u0446\
  \u0456\u043B\u0435 \u0447\u0438\u0441\u043B\u043E, \u044F\u043A\u0435 \u043F\u0440\
  \u0435\u0434\u0441\u0442\u0430\u0432\u043B\u044F\u0454 \u043A\u0456\u043B\u044C\u043A\
  \u0456\u0441\u0442\u044C \u0441\u0438\u043C\u0432\u043E\u043B\u0456\u0432 \u0443\
  \u2026"
lastmod: '2024-03-13T22:44:48.993287-06:00'
model: gpt-4-0125-preview
summary: "\u0423 VBA \u0444\u0443\u043D\u043A\u0446\u0456\u044F `Len` \u0454 \u0432\
  \u0430\u0448\u0438\u043C \u0433\u043E\u043B\u043E\u0432\u043D\u0438\u043C \u0456\
  \u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u043E\u043C \u0434\u043B\u044F\
  \ \u0437\u043D\u0430\u0445\u043E\u0434\u0436\u0435\u043D\u043D\u044F \u0434\u043E\
  \u0432\u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430."
title: "\u0417\u043D\u0430\u0445\u043E\u0434\u0436\u0435\u043D\u043D\u044F \u0434\u043E\
  \u0432\u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430"
weight: 7
---

## Як:
У VBA функція `Len` є вашим головним інструментом для знаходження довжини рядка. Вона повертає ціле число, яке представляє кількість символів у вказаному рядку. Ось простий приклад, щоб ілюструвати цю функцію:

```vb
Sub StringLengthDemo()
    Dim exampleString As String
    exampleString = "Hello, World!"
    ' Знаходження та відображення довжини рядка
    MsgBox Len(exampleString) ' Відображає: 13
End Sub
```

У наведеному вище фрагменті `Len(exampleString)` оцінюється як 13, яке потім відображається з допомогою `MsgBox`.

Для більш практичного застосування розгляньте сценарій, коли ви ітеруєте через колекцію рядків, обробляючи їх на основі їх довжини:

```vb
Sub ProcessStringsBasedOnLength()
    Dim stringCollection(2) As String
    Dim i As Integer
    
    ' Приклад рядків
    stringCollection(0) = "VBA"
    stringCollection(1) = "Visual Basic for Applications"
    stringCollection(2) = "!"

    For i = LBound(stringCollection) To UBound(stringCollection)
        If Len(stringCollection(i)) > 5 Then
            MsgBox "Довгий рядок: " & stringCollection(i)
        Else
            MsgBox "Короткий рядок: " & stringCollection(i)
        End If
    Next i
End Sub
```

Цей код класифікуватиме кожен рядок у `stringCollection` як "Довгий рядок" або "Короткий рядок", залежно від того, чи його довжина більша за 5 символів.

## Поглиблений огляд
Функція `Len` у VBA має свої корені у ранньому програмуванні BASIC, забезпечуючи простий, але ефективний спосіб для виконання завдань маніпулювання рядками. З роками, у міру розвитку мов програмування, багато з них розробили більш вдосконалені інструменти для роботи з рядками, такі як регулярні вирази та комплексні бібліотеки маніпулювання рядками.

Однак, у контексті VBA, `Len` залишається основним та високоефективним рішенням для визначення довжини рядка — частково через зосередження VBA на простоті використання та доступності на відміну від складності операцій. Хоча мови, такі як Python або JavaScript, пропонують методи, як `.length` або `len()`, вбудовані безпосередньо в об'єкти рядка, функція `Len` VBA вирізняється своїм прямим застосуванням, особливо корисним для тих, хто лише починає занурюватися у світ програмування з таких галузей, як аналіз даних або автоматизація офісу.

Варто відзначити, що хоча функція `Len` зазвичай достатня для більшості сценаріїв, пов'язаних із визначенням довжини рядка в VBA, для більш складних маніпуляцій, що включають юнікодні рядки або обробку рядків з різноманітними наборами символів, можуть знадобитися альтернативні методи. itemprop="alternative">У цих випадках інші програмні середовища або додаткові функції бібліотеки VBA можуть запропонувати більш надійні рішення. Проте, для переважної більшості завдань у сфері VBA, `Len` ефективно виконує своє завдання, продовжуючи свою традицію як основного засобу маніпулювання рядками.
