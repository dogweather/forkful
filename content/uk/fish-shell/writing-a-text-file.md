---
title:                "Fish Shell: Написання текстового файлу"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Написання текстового файлу є неодмінною частиною програмування в оболонці Fish Shell. Відкриваємо Shell, починаємо писати код і виконуємо потрібні команди, але якщо все це зберегти в текстовий файл, то ми отримуємо набагато більш зручний та організований спосіб збереження нашої роботи.

## Як

Використовуйте команду `echo` для створення та запису інформації в текстовий файл. Потім використовуйте команду `cat` для перегляду вмісту файлу. Також, можна використовувати ці команди в поєднанні з іншими командами Shell для створення та редагування більш складних текстових файлів.

```Fish Shell
echo "Привіт, світ!" > hello.txt
cat hello.txt
```
Output: `Привіт, світ!` 

## Глибоке погруження

Іноді, нам потрібно зберегти більше ніж один рядок тексту. Для цього, використовуйте команду `printf` з опцією `-e` для додавання символів нового рядка (`\n`). Також, можна використовувати спеціальні символи, такі як `>` для додавання тексту до кінця файлу і `>>` для додавання тексту на початок файлу.

```Fish Shell
printf -e "Рядок 1\nРядок 2" > lines.txt
echo "Додатковий рядок" >> lines.txt
cat lines.txt
```
Output: 
```
Рядок 1
Рядок 2
Додатковий рядок
```

## Дивіться також

- [Офіційна документація Fish Shell](https://fishshell.com/docs/current/)
- [Основи програмування в оболонці Fish Shell](https://www.digitalocean.com/community/tutorials/how-to-write-simple-shell-scripts-with-fish)
- [Робота з файлами та каталогами в Fish Shell](https://towardsdatascience.com/how-to-use-files-in-fish-shell-scripts-5d52ab508691)