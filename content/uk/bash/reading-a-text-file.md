---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що та чому?

Читання текстового файла - це процес зчитування даних з файла, який складається з тексту. Програмісти роблять це для отримання, обробки та аналізу даних.

## Як зробити:

В bash можна використовувати команду `cat` або зчитувати файл по рядках усередині скрипта. Ось як це працює:

Базове читання файлу:

```bash
cat my_text_file.txt
```
Цей код виведе весь вміст `my_text_file.txt` в консоль.

Зчитування файлу по рядках:

```bash
while IFS= read -r line
do
    echo "$line"
done < my_text_file.txt
```
Цей код прочитає кожен рядок з `my_text_file.txt` і виведе його в консоль.

## Поглиблений огляд

Bash (Bourne Again Shell) був створений для ГНУ-проекту як вільна заміна Bourne Shell (sh) в 1989 році. Він дозволяє вам не лише читати файли, але й редагувати їх, використовуючи всією потужність shell scripting.

Альтернативи bash існують і залежать від ваших потреб. Наприклад, Python або Perl можуть бути кращим вибором для складнішого аналізу даних.

Читання файлу у Bash здійснюється за допомогою системних викликів, таких як `read` або `cat`, що використовують операційній системи для зчитування даних із файла.

## Дивіться також:

- Довідка Bash: https://www.gnu.org/software/bash/manual/bash.html
- Вікіпедія про Bash: https://uk.wikipedia.org/wiki/Bash
- Читання та записування файлів в Bash: https://linuxize.com/post/bash-read-file/