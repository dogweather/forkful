---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:01.493673-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 VBA \u0444\u0443\u043D\u043A\u0446\u0456\u0457 \u0432\u0438\u0437\u043D\
  \u0430\u0447\u0430\u044E\u0442\u044C\u0441\u044F \u0437\u0430 \u0434\u043E\u043F\
  \u043E\u043C\u043E\u0433\u043E\u044E \u043E\u043F\u0435\u0440\u0430\u0442\u043E\u0440\
  \u0456\u0432 `Function` \u0442\u0430 `End Function`. \u041E\u0441\u044C \u043F\u0440\
  \u043E\u0441\u0442\u0438\u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434 \u0442\
  \u043E\u0433\u043E, \u044F\u043A \u0441\u0442\u0432\u043E\u0440\u0438\u0442\u0438\
  \ \u0444\u0443\u043D\u043A\u0446\u0456\u044E, \u044F\u043A\u0430\u2026"
lastmod: '2024-03-13T22:44:49.019473-06:00'
model: gpt-4-0125-preview
summary: "\u0423 VBA \u0444\u0443\u043D\u043A\u0446\u0456\u0457 \u0432\u0438\u0437\
  \u043D\u0430\u0447\u0430\u044E\u0442\u044C\u0441\u044F \u0437\u0430 \u0434\u043E\
  \u043F\u043E\u043C\u043E\u0433\u043E\u044E \u043E\u043F\u0435\u0440\u0430\u0442\u043E\
  \u0440\u0456\u0432 `Function` \u0442\u0430 `End Function`."
title: "\u041E\u0440\u0433\u0430\u043D\u0456\u0437\u0430\u0446\u0456\u044F \u043A\u043E\
  \u0434\u0443 \u0443 \u0444\u0443\u043D\u043A\u0446\u0456\u0457"
weight: 18
---

## Як це зробити:
У VBA функції визначаються за допомогою операторів `Function` та `End Function`. Ось простий приклад того, як створити функцію, яка обчислює площу прямокутника:

```basic
Function CalculateArea(length As Double, width As Double) As Double
    CalculateArea = length * width
End Function
```

Для виклику цієї функції у вашому коді VBA та відображення результату у діалоговому вікні, ви повинні використовувати:

```basic
Sub ShowArea()
    Dim area As Double
    area = CalculateArea(10, 5)
    MsgBox "Площа є " & area
End Sub
```

При виконанні цей код показує діалогове вікно з повідомленням: `Площа є 50`.

### Передача змінних ByRef та ByVal
VBA дозволяє передавати змінні до функцій або за посиланням (`ByRef`), або за значенням (`ByVal`). Перше означає, що оригінальна змінна може бути змінена функцією, тоді як друге передає копію, захищаючи оригінальну змінну від змін.

```basic
Function ModifyValue(ByRef num As Integer)
    num = num + 5
End Function

Function PreserveValue(ByVal num As Integer) As Integer
    num = num + 5
    PreserveValue = num
End Function
```

## Поглиблений Огляд
VBA, як мова програмування, орієнтована на події, приділяє значну увагу функціям і підпрограмам для вирішення різних завдань. На відміну від багатьох сучасних мов, VBA має унікальну особливість, де ключове слово `Function` не тільки заявляє блок повторно використовуваного коду, але й дозволяє неявне повернення значення, яке прямо призначено імені функції.

Історично дизайн функцій VBA був під впливом більш ранніх парадигм програмування, де енкапсуляція та модульність поступово визнавалися за їх важливість у розробці програмного забезпечення. Цей історичний фон привів VBA до прийняття дещо консервативного, але функціонального підходу до організації коду.

Хоча VBA могутній у своїх рідних середовищах (наприклад, додатки Microsoft Office), важливо зазначити, що світ програмування розвинувся. Мови програмування, такі як Python, пропонують більш простий синтаксис і велику стандартну бібліотеку, роблячи їх вигідною альтернативою для різних застосунків поза набором Office. Тим не менш, при роботі в продуктах Microsoft Office, можливості інтеграції та автоматизації, які забезпечує VBA, є неперевершеними.

Варто відзначити, що незважаючи на свій вік, спільнота навколо VBA залишається активною, постійно знаходячи інноваційні способи використання його функціональності. Проте, оскільки програмна індустрія рухається до більш сучасних, універсальних і надійних мов, програмістам, знайомим з VBA, рекомендується розглядати ці альтернативи для завдань, не пов’язаних з Office, щоб розширити свій інструментарій кодування.
