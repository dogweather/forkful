---
title:                "Конкатенація рядків"
html_title:           "PHP: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Що таке конкатенація рядків? Це об'єднання двох або більше рядків у один. Програмісти роблять це для об'єднання даних, обробки вводу користувача та створення повідомлень.

## Як це зробити:

Об'єднайте рядки в Bash просто помістивши їх біля одного одного:

```Bash
string1="Привіт, "
string2="світ!"
greeting=$string1$string2
echo $greeting
```

Вихід буде таким:

```Bash
Привіт, світ!
```

## Занурення в деталі:

Конкатенація рядків в Bash ніяк не пов'язана з раннім язиком Bourne shell. Однак, ця функція була доступна в більшості ранніх версій Unix shell.

Альтернативи конкатенації рядків в Bash включають команду `printf` і використання утиліти `cat`. Але обидві ці методи вимагають більше коду й потребують додаткового виклику процесу. 

Розробники Bash реалізують конкатенацію рядків простим об'єднанням значень змінних.

## Дивіться також:

[Bash String Concatenation - Stack Overflow](https://stackoverflow.com/questions/4181703/bash-string-concatenation)

[Bash Guide for Beginners - The Linux Documentation Project](http://tldp.org/LDP/Bash-Beginners-Guide/html/index.html)

[String manipulation in Bash - WikiBooks](https://en.wikibooks.org/wiki/Bash_Shell_Scripting/String_Manipulations)