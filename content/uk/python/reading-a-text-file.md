---
title:                "Python: Читання текстового файлу."
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Читання текстового файлу є важливою частиною програмування в Python. Це дозволяє робити обробку великої кількості даних з мінімальним зусиллям.

## Як

```Python
# Відкриваємо файл для читання
file = open("text_file.txt", "r")

# Зчитуємо весь вміст файлу і зберігаємо в змінну
content = file.read()

# Виводимо зміст файлу
print(content)

# Закриваємо файл
file.close()
```

Вивід: Зміст, який був записаний в файлі.

```Python
# Відкриваємо файл для читання
file = open("text_file.txt", "r")

# Зчитуємо по одному рядку і виводимо
for line in file:
  print(line)

# Закриваємо файл
file.close()
```

Вивід: Кожен рядок із файлу буде виведений по черзі.

## Глибокий захоплення

Крім простої операції читання файлу, Python також надає можливість використовувати різні методи для обробки даних. Наприклад, `readlines()` дозволяє зчитати всі рядки в файлі і повертає їх у вигляді списку. Також, використання `with` конструкції автоматично закриє файл після виконання.

```Python
# Зчитуємо всі рядки файлу
with open("text_file.txt", "r") as file:
  lines = file.readlines()

# Виводимо списком
print(lines)
```

Вивід: Список із всіма рядками файлу.

## Дивись також

- [Документація Python про роботу з файлами](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Приклади читання текстового файлу в Python](https://www.geeksforgeeks.org/reading-writing-text-files-python/)