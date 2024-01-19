---
title:                "Видалення символів, що відповідають патерну"
html_title:           "Arduino: Видалення символів, що відповідають патерну"
simple_title:         "Видалення символів, що відповідають патерну"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Видалення символів, які відповідають певному шаблону, це процес вилучення певних символів з рядка в Bash. Програмісти роблять це для обробки тексти, видалення небажаних символів, перетворення форматів файлів тощо.

## Як?

Ось код, який шукає і видаляє символи, що відповідають шаблону, від імені файлу:

```Bash
filename="example.txt"
new_filename="${filename//txt/jpg}"
echo $new_filename
```

Виходить результат:

```Bash
example.jpg
```

## Поглиблений розгляд 

Дана функція була є частиною Bash з його першої версії. Існують альтернативи як `tr` і `sed`, але подвійні знаки заміни `//` - найпростіший спосіб заміни у Bash. Це реалізовано через використання певного внутрішнього алгоритму Bash для пошуку та видалення зібраних рядків.

## Дивіться також 

[Bash String Manipulation](https://www.thegeekstuff.com/2010/07/bash-string-manipulation/)
[Understanding & Using Bash Shell Built-In 'echo' Command](https://www.linuxshelltips.com/bash-echo-command/)