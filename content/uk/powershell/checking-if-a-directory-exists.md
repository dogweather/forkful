---
title:                "Перевірка наявності директорії"
html_title:           "PowerShell: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що і чому?

Перевірка існування директорії є важливою задачею при програмуванні, оскільки дозволяє впевнитися в наявності або відсутності необхідного шляху на комп'ютері. Це забезпечує точність та безпеку виконання програм.

## Як?

За допомогою команди "Test-Path" можна перевірити існування директорії в PowerShell. Нижче наведені приклади коду та відповідний вихід для ілюстрації цього:

```PowerShell
Test-Path C:\Users -PathType Any
```
Виведе результат "True", оскільки директорія "Users" існує на вашому комп'ютері.

```PowerShell
Test-Path C:\Program Files (x86)\Microsoft Office -PathType Container
```
Виведе результат "False", оскільки директорія "Microsoft Office" не існує в директорії "Program Files (x86)".

```PowerShell
Test-Path D:\Documents -PathType Leaf
```
Виведе результат "False", оскільки директорія "Documents" не є файловою структурою, а отже не є кінцевою точкою шляху.

## Deep Dive

Перевірка існування директорії є стандартною процедурою в більшості мов програмування, включаючи PowerShell. Це забезпечує безпечне виконання програм, оскільки дозволяє програмістам уникнути спроб доступу до неіснуючих директорій, що може призвести до помилок або витоку інформації. Альтернативою команді "Test-Path" може бути використання функцій "Get-ChildItem" або "Get-Item", що також дозволяють отримати інформацію про існування директорій. Різним параметрам команди "Test-Path" можна задавати виконання різних видів перевірок, наприклад, чи є даний шлях директорією, файлом, або навіть посиланням. Додаткову інформацію про використання команди можна знайти в офіційній документації PowerShell.

## See Also

- [Документація PowerShell на тему "Test-Path"](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path)
- [Стаття "PowerShell для початківців"](https://docs.microsoft.com/en-us/learn/modules/collect-computer-evidence-powershell-introduction/)
- [Інструкція "Використання команд для роботи з файлами та директоріями в PowerShell"](https://blog.netwrix.com/2018/08/06/powershell-file-directory-commands/)