---
title:                "Робота з CSV"
aliases:
- /uk/vba/working-with-csv.md
date:                  2024-02-01T22:05:43.894860-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/vba/working-with-csv.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

Робота з файлами CSV (Comma Separated Values - Значення, Розділені Комами) включає читання з текстових файлів або запис до них, де поля даних розділені комами. Програмісти часто виконують цю задачу, щоб полегшити обмін даними між різними програмними застосунками, враховуючи простоту та широке прийняття формату CSV в різних програмних середовищах.

## Як це здійснити:

Visual Basic for Applications (VBA) спрощує роботу з файлами CSV за допомогою вбудованих функцій та методів, які безперешкодно дозволяють читати з цих файлів і записувати до них. Нижче наведено приклади, що ілюструють базові операції з файлами CSV.

### Читання з файлу CSV:

```basic
Sub ReadCSV()
    Dim filePath As String
    filePath = "C:\example.csv"
    
    Open filePath For Input As #1
    
    Do Until EOF(1)
        Dim line As String
        Line Input #1, line
        Dim dataFields() As String
        dataFields = Split(line, ",")
        
        'Обробка масиву dataFields за потреби
        Debug.Print Join(dataFields, ";") 'Приклад виводу з конвертацією ком на крапки з комою
    Loop
    
    Close #1
End Sub
```

### Запис у файл CSV:

```basic
Sub WriteCSV()
    Dim filePath As String
    filePath = "C:\output.csv"
    Dim dataToWrite As String
    dataToWrite = "ID,Name,Age" & vbCrLf & "1,John Doe,30" & vbCrLf & "2,Jane Doe,29"
    
    Open filePath For Output As #1
    Print #1, dataToWrite
    Close #1
End Sub
```

Приклад виводу в `output.csv`:
```
ID,Name,Age
1,John Doe,30
2,Jane Doe,29
```

## Поглиблений огляд

Історично файли CSV були простим способом зберігання табличних даних у текстовому форматі. Простота його структури, де кожен рядок відповідає одному запису даних і кожне поле в межах запису розділене комою, є як сила, так і обмеження CSV. Формат не підтримує типи даних нативно, що означає, що всі дані зберігаються як рядки, і тягар конвертації даних у правильний тип лягає на програміста.

У Visual Basic for Applications робота з файлами CSV переважно виконується через базові операції з файлами, як показано в попередніх прикладах. Немає прямої підтримки аналізу CSV як у більш сучасних мовах (наприклад, модуль csv в Python), який надає більше контролю та зручності при роботі з даними CSV.

Для більш складних операцій або при роботі з великими файлами CSV програмісти можуть знайти кращі альтернативи поза чистим VBA, такі як використання зовнішніх бібліотек або застосування інших програмних мов, оснащених більш складними можливостями обробки CSV. Однак, для простих завдань, пов’язаних з файлами CSV, прямолінійний підхід VBA часто достатньо простий і легкий у впровадженні, пропонуючи швидке рішення для застосунків на основі Excel або іншої автоматизації програмного забезпечення Microsoft Office.
