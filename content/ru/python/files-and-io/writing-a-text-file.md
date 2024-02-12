---
title:                "Создание текстового файла"
date:                  2024-01-29T00:05:51.603172-07:00
model:                 gpt-4-0125-preview
simple_title:         "Создание текстового файла"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/python/writing-a-text-file.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Запись текстового файла в Python означает сохранение строк в файл на вашем диске. Программисты делают это для сохранения данных между сессиями, логирования информации или экспорта читабельных результатов.

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