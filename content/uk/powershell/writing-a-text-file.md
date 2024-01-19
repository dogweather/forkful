---
title:                "Написання текстового файлу"
html_title:           "PowerShell: Написання текстового файлу"
simple_title:         "Написання текстового файлу"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

Українським програмістам привіт! Сьогодні ми розглянемо, як за допомогою PowerShell можна записувати текстові файли. Це корисний навичка, яка допоможе вам зберігати та обробляти інформацію на вашому комп'ютері.

## Що і чому?
Запис текстового файлу - це процес створення файла, який містить лише текстові дані. Це може бути будь-який вид інформації - від звичайних рядків до таблиць та коду. Програмісти часто записують текстові файли, щоб зберігати код своїх програм чи встановлювати налаштування для різних інструментів. Це зроблено для полегшення доступу до цих даних у майбутньому.

## Як це зробити?
Записати текстовий файл за допомогою PowerShell дуже просто. Ми можемо скористатися командою `Out-File` та вказати назву файла та потрібний текст у наступному форматі:

```
PowerShell Set-Content 'назва_файла.txt' -value 'це текст, який буде записаний в файл'
```

Обов'язково використовуйте кавички, щоб вказати текст. Якщо хочете додати новий рядок у файл, то потрібно використати команду `Add-Content` замість `Set-Content`.

Якщо потрібно записати більш складні дані, такі як таблиці або об'єкти, можна скористатися командою `Export-Csv`.

## Поглиблене вивчення
За допомогою PowerShell можна не тільки записувати, але і читати та редагувати текстові файли. Це був один з основних функцій мови з самого початку її розробки у 2006 році. Альтернативними засобами запису текстових файлів є вбудовані у Windows програми, такі як блокнот або WordPad.

Також, можливе використання PowerShell для маніпуляції з даними в інших форматах, наприклад, Excel або XML. Для цього потрібно вчитися використовувати різні команди та функції.

Щоб дізнатися більше про роботу з текстовими файлами, рекомендую ознайомитися з офіційною документацією [PowerShell на сайті Microsoft](https://docs.microsoft.com/powershell/scripting/).

## Додаткові посилання
- [Робота з файлами і папками в PowerShell на Dev.ua](https://devua.net/programirovanie/310-powershell-rabota-s-fajlami-i-papkami.html)
- [Навчальний курс Microsoft про роботу з файлами у PowerShell](https://mva.microsoft.com/en-us/training-courses/getting-started-with-microsoft-powershell-8276)