---
title:                "Python: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Написання текстового файлу - важлива навичка, яка дає можливість зберігати і організовувати дані для подальшого використання. Вона також дозволяє зберігати дані в структурованому форматі, що полегшує подальшу обробку і аналіз.

## Як це зробити

Щоб створити текстовий файл у Python, використовуйте вбудовану функцію `open()` та метод `write()`:

```Python
file = open("new_file.txt", "w") # `w` означає, що ми створюємо файл для запису
file.write("Це текст, який буде записаний у файл")
file.close()
```

Ще один важливий етап - це закрити файл після закінчення запису, щоб зберегти зміни. Для зручності можна також використати оператор `with`, який автоматично закриє файл після закінчення виконання інструкцій:

```Python
with open("new_file.txt", "w") as file:
    file.write("Це текст, який буде записаний у файл")
```

## Глибокий занурення

У Python є кілька параметрів, які можна використати при створенні текстового файлу. Наприклад, ви можете вказати кодування файлу, щоб він коректно відображав різні символи:

```Python
with open("new_file.txt", "w", encoding="utf-8") as file:
    file.write("Це текст, який буде записаний у файл")
```

Також можна використати метод `read()` для отримання вмісту файлу:

```Python
with open("new_file.txt", "r") as file: # `r` означає, що ми читаємо файл
    contents = file.read()
```

Щоб прочитати лише певну кількість рядків, можна використати метод `readlines()`:

```Python
with open("new_file.txt", "r") as file:
    lines = file.readlines(2) # отримаємо перші 2 рядки
```

## Дивіться також

- [Документація з файлів у Python](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Стаття про роботу зі структурованими даними у Python](https://realpython.com/python-data-structures/)
- [Курси з основ програмування у Python](https://geekbrains.ua/courses/programming-basics)