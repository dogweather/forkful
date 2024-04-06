---
date: 2024-01-20 17:53:55.673514-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u041F\
  \u0440\u0438\u043F\u0443\u0441\u0442\u0438\u043C\u043E, `myfile.txt` \u043C\u0456\
  \u0441\u0442\u0438\u0442\u044C."
lastmod: '2024-04-05T21:53:49.740678-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u0440\u0438\u043F\u0443\u0441\u0442\u0438\u043C\u043E, `myfile.txt`\
  \ \u043C\u0456\u0441\u0442\u0438\u0442\u044C."
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

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
