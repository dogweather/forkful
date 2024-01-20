---
title:                "Створення тимчасового файлу"
html_title:           "C: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Що та чому?

Створювати тимчасовий файл - це значить налаштувати місце для короткочасного зберігання даних. Програмісти роблять це для обміну даними між процесами, тестування та інших зручностей.

## Як це зробити:

Тимчасовий файл можна створити за допомогою `mktemp`. Ось код та приклад виводу:

```Bash
tempfile=`mktemp`
echo "Ми зберігаємо деякі дані тут." > $tempfile
cat $tempfile
```
Виведення:

```Bash
Ми зберігаємо деякі дані тут.
```
## Більше інформації:

Тимчасові файли використовуються протягом довгого часу в Unix-подібних ОС, включаючи Linux та macOS. Існує кілька альтернатив `mktemp`, таких як `TMPFILE=$(tempfile)`. Однак `mktemp` вважається більш надійним, оскільки він автоматично генерує унікальне ім'я файла. 

## Дивіться також:

- Bash Scripting Tutorial: [Процедура створення тимчасових файлів](https://linuxconfig.org/bash-scripting-tutorial)