---
title:                "Використання асоціативних масивів"
aliases: - /uk/vba/using-associative-arrays.md
date:                  2024-02-01T22:04:58.146452-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання асоціативних масивів"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/vba/using-associative-arrays.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Асоціативні масиви, які часто називають словниками в Visual Basic для додатків (VBA), дозволяють програмістам створювати колекції пар ключ-значення. Ця особливість є вирішальною для ефективного зберігання та отримання даних, пропонуючи більш гнучкий та інтуїтивно зрозумілий спосіб управління даними, ніж традиційні індекси масивів.

## Як це зробити:

У VBA об'єкт `Dictionary` надає функціональність, схожу на асоціативні масиви. Спочатку вам потрібно додати посилання на Microsoft Scripting Runtime, щоб його використовувати:

1. У редакторі VBA перейдіть до Tools > References...
2. Встановіть галочку на "Microsoft Scripting Runtime" та натисніть OK.

Ось як оголосити, заповнити та отримати доступ до елементів у `Dictionary`:

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' Додавання елементів
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Інженер"

' Доступ до елементів
Debug.Print sampleDictionary.Item("Name")  ' Вивід: John Doe
Debug.Print sampleDictionary.Item("Age")   ' Вивід: 29

' Перевірка наявності ключа
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Ключ Occupation існує"
End If

' Видалення елементів
sampleDictionary.Remove("Occupation")

' Проходження через словник
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## Поглиблене вивчення

Об'єкт `Dictionary` під капотом взаємодіє з компонентами Windows Scripting Host. Як такий, це пізньо зв'язаний COM-об'єкт, який був поширений спосіб розширення функціональності VBA в минулому. Його використання у VBA може значно підвищити здатність мови маніпулювати складними наборами даних без застосування жорсткої структури, як це бачимо в традиційних масивах або діапазонах Excel.

Однією з обмежень, про які варто пам'ятати, є те, що доступ до `Dictionary` вимагає встановлення посилання на Microsoft Scripting Runtime, що може ускладнити розповсюдження ваших проектів VBA. Альтернативи, такі як колекції, існують у межах VBA, але не мають деяких ключових особливостей `Dictionary`, таких як можливість легко перевіряти наявність ключа без виклику помилки.

У більш сучасних програмних контекстах мови, такі як Python, пропонують вбудовану підтримку асоціативних масивів (також відомих як словники і в Python), без необхідності додавання зовнішніх посилань. Ця вбудована підтримка спрощує процес та пропонує більш розширені функції "з коробки". Тим не менш, у межах VBA і для конкретних застосувань, спрямованих на автоматизацію задач у наборі Microsoft Office, використання об'єкта `Dictionary` залишається потужним і актуальним методом для асоціативних масивоподібних структур даних.
