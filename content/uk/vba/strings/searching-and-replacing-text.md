---
title:                "Пошук та заміна тексту"
aliases:
- /uk/vba/searching-and-replacing-text/
date:                  2024-02-01T22:02:25.733267-07:00
model:                 gpt-4-0125-preview
simple_title:         "Пошук та заміна тексту"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/vba/searching-and-replacing-text.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Пошук та заміна тексту в Visual Basic for Applications (VBA) є необхідним для програмного редагування документів, таблиць і баз даних. Ця можливість дозволяє програмістам автоматизувати масові виправлення, коригування помилок або оновлення інформації в великих наборах даних без ручного втручання.

## Як це робити:

У VBA пошук та заміна тексту може бути реалізовано за допомогою функції `Replace` або через специфічні моделі об'єктів у таких програмах як Excel або Word. Нижче наведено приклади, що ілюструють обидва підходи.

### Використання функції `Replace`:

Функція `Replace` є простою для простих замін тексту. Вона має форму `Replace(expression, find, replaceWith[, start[, count[, compare]]])`.

Приклад:
```vb
Dim originalText As String
Dim newText As String

originalText = "Hello, World! Programming in VBA is fun."
newText = Replace(originalText, "World", "Everyone")

Debug.Print newText
```
Виведення:
```
Hello, Everyone! Programming in VBA is fun.
```

### Пошук та заміна в Excel:

Для Excel ви можете використовувати метод `Range.Replace`, який надає більше контролю, такого як чутливість до регістру та заміну цілих слів.

Приклад:
```vb
Sub ReplaceTextInExcel()
    Dim ws As Worksheet
    Set ws = ThisWorkbook.Sheets("Sheet1")

    With ws.Range("A1:A100") ' Визначення діапазону, в якому ви хочете шукати
        .Replace What:="old", Replacement:="new", MatchCase:=False, LookAt:=xlPart
    End With
End Sub
```

### Пошук та заміна в Word:

Аналогічно, Word має потужну функцію `Find` та `Replace`, доступну через VBA.

Приклад:
```vb
Sub ReplaceTextInWord()
    Dim doc As Document
    Set doc = ActiveDocument
    
    With doc.Content.Find
        .Text = "specific"
        .Replacement.Text = "particular"
        .Execute Replace:=wdReplaceAll
    End With
End Sub
```

## Поглиблений огляд:

Пошук та заміна тексту в VBA повертає нас до ранніх можливостей автоматизації в додатках Microsoft Office, значно підвищуючи продуктивність за рахунок скриптів для повторюваних задач. З часом, ці функції еволюціонували, ставши більш потужними та гнучкими, і підтримуючи широкий спектр випадків застосування.

Хоча функція `Replace` у VBA зручна для простих текстових операцій, моделі об'єктів Excel та Word надають більший контроль і повинні використовуватись для завдань, специфічних для додатків. Вони підтримують розширені можливості, такі як узгодження за шаблоном, збереження форматування, та нюансовані критерії пошуку (наприклад, врахування регістру, цілих слів).

Проте, VBA та його можливості маніпулювання текстом, хоч і робастні в екосистемі Microsoft, можуть не завжди бути найкращим інструментом для обробки тексту високої продуктивності або для більш складних потреб. Мови, такі як Python, з бібліотеками типу `re` для регулярних виразів, пропонують більш потужні та універсальні варіанти маніпулювання текстом. Але для тих, хто вже працює в додатках Microsoft Office, VBA залишається доступним та ефективним вибором для автоматизації завдань пошуку та заміни.
