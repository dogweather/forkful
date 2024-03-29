---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:15.774638-07:00
description: "\u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043D\u0430\u043B\
  \u0430\u0433\u043E\u0434\u0436\u0443\u0432\u0430\u043B\u044C\u043D\u0438\u0445 \u0434\
  \u0430\u043D\u0438\u0445 \u0443 Visual Basic \u0434\u043B\u044F \u0434\u043E\u0434\
  \u0430\u0442\u043A\u0456\u0432 (VBA) \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\
  \u0430\u0454 \u0441\u0442\u0440\u0430\u0442\u0435\u0433\u0456\u0447\u043D\u0435\
  \ \u0440\u043E\u0437\u043C\u0456\u0449\u0435\u043D\u043D\u044F \u043E\u043F\u0435\
  \u0440\u0430\u0442\u043E\u0440\u0456\u0432 \u0434\u0440\u0443\u043A\u0443 \u0443\
  \ \u0432\u0430\u0448\u043E\u043C\u0443 \u043A\u043E\u0434\u0456, \u0449\u043E\u0431\
  \ \u0432\u0456\u0434\u043E\u0431\u0440\u0430\u0437\u0438\u0442\u0438\u2026"
lastmod: '2024-03-13T22:44:49.014277-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043D\u0430\u043B\
  \u0430\u0433\u043E\u0434\u0436\u0443\u0432\u0430\u043B\u044C\u043D\u0438\u0445 \u0434\
  \u0430\u043D\u0438\u0445 \u0443 Visual Basic \u0434\u043B\u044F \u0434\u043E\u0434\
  \u0430\u0442\u043A\u0456\u0432 (VBA) \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\
  \u0430\u0454 \u0441\u0442\u0440\u0430\u0442\u0435\u0433\u0456\u0447\u043D\u0435\
  \ \u0440\u043E\u0437\u043C\u0456\u0449\u0435\u043D\u043D\u044F \u043E\u043F\u0435\
  \u0440\u0430\u0442\u043E\u0440\u0456\u0432 \u0434\u0440\u0443\u043A\u0443 \u0443\
  \ \u0432\u0430\u0448\u043E\u043C\u0443 \u043A\u043E\u0434\u0456, \u0449\u043E\u0431\
  \ \u0432\u0456\u0434\u043E\u0431\u0440\u0430\u0437\u0438\u0442\u0438\u2026"
title: "\u0414\u0440\u0443\u043A \u0432\u0456\u0434\u043B\u0430\u0433\u043E\u0434\u0436\
  \u0443\u0432\u0430\u043B\u044C\u043D\u043E\u0433\u043E \u0432\u0438\u0432\u043E\u0434\
  \u0443"
---

{{< edit_this_page >}}

## Що та чому?
Виведення налагоджувальних даних у Visual Basic для додатків (VBA) передбачає стратегічне розміщення операторів друку у вашому коді, щоб відобразити значення змінних, послідовність виконання або спеціальні повідомлення для налагодження. Ця техніка є важливою для налагодження, оскільки дозволяє програмістам розуміти поведінку їхнього коду під час виконання та ідентифікувати будь-яку неочікувану поведінку або помилки.

## Як це зробити:
У VBA оператор `Debug.Print` є основним засобом для виведення налагоджувальної інформації до Негайного вікна у редакторі Visual Basic (VBE). Для ефективного використання цієї функції необхідно мати видиме Негайне вікно (Перегляд > Негайне вікно або натиснути `Ctrl+G` у VBE).

Ось простий приклад використання `Debug.Print` для виведення значення змінної та спеціального повідомлення:

```basic
Sub PrintDebugInfo()
    Dim sampleVar As Integer
    sampleVar = 42
    Debug.Print "Значення sampleVar: "; sampleVar
End Sub
```

Коли ви запустите цю підпрограму, у Негайному вікні буде відображено:
```
Значення sampleVar: 42
```

Також можна використовувати його для відстеження потоку складної умовної логіки, вставляючи оператори `Debug.Print` у різні гілки вашого коду:

```basic
Sub CheckValue()
    Dim valueToCheck As Integer
    valueToCheck = 9
    
    If valueToCheck > 10 Then
        Debug.Print "Значення більше ніж 10."
    ElseIf valueToCheck < 10 And valueToCheck > 0 Then
        Debug.Print "Значення між 1 і 9."
    Else
        Debug.Print "Значення 10 або менше ніж 1."
    End If
End Sub
```

Запуск `CheckValue` дає наступне:
```
Значення між 1 і 9.
```

Запам’ятайте, вихідні дані з `Debug.Print` йдуть лише в Негайне вікно, що є надзвичайно корисним під час розробки, але не з’являється в жодній частині програми, яка є передбачена для користувача.

## Поглиблений огляд
Негайне вікно та метод `Debug.Print` мають глибокі корені в історії Visual Basic для додатків, відображаючи еволюцію практик налагодження з часом. Спочатку налагодження було більш текстовим і менш візуальним процесом, з розробниками, які сильно покладалися на оператори друку для розуміння того, що робив їх код. Протягом років, по мірі розвитку середовищ розробки, еволюціонували і засоби налагодження, представляючи точки зупину, спостерігання та більш розширені інструменти профілювання, які надають більш інтерактивне та негайне розуміння поведінки коду.

Тим не менш, `Debug.Print` та Негайне вікно все ще надзвичайно корисні, зокрема для швидких налагоджувань або при роботі з кодом, у який важко увійти (наприклад, обробники подій). Втім, важливо визнати, що покладання виключно на оператори друку для налагодження у сучасному програмуванні може бути менш ефективним порівняно з використанням інтегрованих налагоджувачів з можливостями точок зупину, спостереження та перегляду стеку.

Хоча альтернативи, такі як фреймворки для логування чи більш передові інструменти налагодження, пропонують більше можливостей та гнучкості, простота та негайність використання `Debug.Print` у VBA роблять його цінним інструментом, особливо для програмістів, які переходять з інших мов і вже звикли до технік налагодження на основі друку. Однак, стаючи більш комфортними з VBA та редактором Visual Basic, дослідження повного спектру доступних засобів налагодження може призвести до більш ефективного та ефективного вирішення проблем.
