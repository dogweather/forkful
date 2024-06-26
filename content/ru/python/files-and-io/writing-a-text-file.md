---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:51.603172-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0417\u0430\u043F\u0438\u0441\u044C \u0432 \u0444\u0430\u0439\u043B\
  \ \u043F\u0440\u043E\u0441\u0442\u0430. \u0418\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u0439\u0442\u0435 \u043A\u043E\u043D\u0441\u0442\u0440\u0443\u043A\u0446\u0438\
  \u044E `with` \u0434\u043B\u044F \u043E\u0442\u043A\u0440\u044B\u0442\u0438\u044F\
  \ \u0444\u0430\u0439\u043B\u0430, \u0430 \u0437\u0430\u0442\u0435\u043C \u0432\u044B\
  \u0437\u043E\u0432\u0438\u0442\u0435 `write()`."
lastmod: '2024-03-13T22:44:44.305733-06:00'
model: gpt-4-0125-preview
summary: "\u0417\u0430\u043F\u0438\u0441\u044C \u0432 \u0444\u0430\u0439\u043B \u043F\
  \u0440\u043E\u0441\u0442\u0430."
title: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 24
---

## Как это сделать:
Запись в файл проста. Используйте конструкцию `with` для открытия файла, а затем вызовите `write()`.

```Python
# Запись в файл на Python
with open('example.txt', 'w') as file:
    file.write("Привет, мир!")

# Чтение файла обратно
with open('example.txt', 'r') as file:
    print(file.read())
```

Пример выходных данных:
```
Привет, мир!
```

Добавление в существующий файл без перезаписи:

```Python
# Добавление в файл на Python
with open('example.txt', 'a') as file:
    file.write("\nДо свидания, мир!")

# Чтение добавленного файла
with open('example.txt', 'r') as file:
    print(file.read())
```

Пример выходных данных:
```
Привет, мир!
До свидания, мир!
```

## Подробнее
Запись текстовых файлов имеет корни в начальных компьютерных системах. Это наиболее базовая форма сохранения данных и обмена между программами и системами. Хотя для комплексных данных существуют альтернативы, такие как базы данных, текстовые файлы широко используются из-за их простоты и читабельности для человека. При записи файлов Python обрабатывает множество сложностей, таких как буферизация и управление памятью, за кулисами и предоставляет различные режимы (например, запись 'w', добавление 'a') для разных случаев использования.

## Смотрите также
- Официальная документация Python по файловому вводу/выводу: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Руководство Real Python по файловому вводу/выводу: https://realpython.com/read-write-files-python/
- Дополнительное чтение о работе с файлами в Python с использованием контекстных менеджеров: https://docs.python.org/3/reference/compound_stmts.html#the-with-statement
