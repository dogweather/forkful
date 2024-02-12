---
title:                "Читання текстового файлу"
aliases:
- /uk/bash/reading-a-text-file/
date:                  2024-01-20T17:53:55.673514-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що та чому?
Читання текстового файлу – це процес отримання даних з файлу, щоб з ними можна було працювати. Програмісти роблять це, щоб обробити, аналізувати чи використовувати інформацію, збережену у файлах.

## Як це робити:
```Bash
# Читання всього файлу за раз
cat myfile.txt

# Читання файлу построково
while IFS= read -r line; do
  echo "Текст рядка: $line"
done < myfile.txt
```
Припустимо, `myfile.txt` містить:
```
Привіт
Це файл
текстових даних
```
Вивід команд буде:
```
Привіт
Це файл
текстових даних
```
та
```
Текст рядка: Привіт
Текст рядка: Це файл
Текст рядка: текстових даних
```

## Поглиблено:
Читання файлів у Bash існує з часів ранніх версій Unix. Команда `cat` була однією з основних. Проте, іноді краще обробляти файл построково, що зменшує використання пам'яті. 

Альтернативи `cat` включають `awk`, `sed`, чи `grep` для специфічних завдань. Наприклад, `awk` чудово підходить для обробки текстових даних на основі шаблонів.

В Bash існує універсальне внутрішнє поле роздільника `IFS` (Internal Field Separator), яке використовують для ітерації по строках файлу без розбивання строк на слова.

## Дивіться також:
- GNU Bash manual: https://www.gnu.org/software/bash/manual/bash.html
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- `awk` programming language: https://www.gnu.org/software/gawk/manual/gawk.html
- `sed` & `grep`: https://www.gnu.org/software/sed/manual/sed.html, http://www.gnu.org/software/grep/manual/grep.html
