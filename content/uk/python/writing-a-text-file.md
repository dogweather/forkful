---
title:                "Створення текстового файлу"
date:                  2024-01-19
html_title:           "Arduino: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що та чому?

Запис текстових файлів - це процес створення та збереження текстової інформації у файлі. Програмісти використовують це для логування, збереження даних та конфігурацій.

## Як зробити:

```python
# Запис у файл
text_data = "Привіт, це текст для запису!"
with open('example.txt', 'w', encoding='utf-8') as file:
    file.write(text_data)

# Перевірка вмісту
with open('example.txt', 'r', encoding='utf-8') as file:
    print(file.read())
```

```python
Привіт, це текст для запису!
```

## Підводне каміння:

Запис файлів у Python - це концепт, що походить від початкових днів програмування. До Python були альтернативи, на кшталт відкритих файлів у C чи письма через командні оболонки. В Python існує також `file.write()` та `file.writelines()`, що дозволяють контролювати запис. Важливо відзначити і про режими запису: 'w' - перезаписує файл, 'a' - дописує в кінець.

## Дивіться також:

- Документація Python: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Путівник по файловим операціям у Python: https://realpython.com/read-write-files-python/
