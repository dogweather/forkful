---
title:                "C: Пошук та заміна тексту"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

Заміна тексту є важливою частиною написання програм, оскільки вона дозволяє легко змінювати та виправляти рядки коду. Це особливо корисно, коли потрібно зробити одну та ж саму зміну в багатьох місцях. В цій статті ми розглянемо, як користуватися функцією пошуку та заміни тексту в мові програмування C.

## Як це зробити

Для того, щоб замінити текст в С, необхідно використовувати функцію `str_replace()`. Синтаксис цієї функції виглядає так:

```C
char *str_replace(char *text, char *from, char *to);
```
Ця функція приймає три параметри: `text` - рядок, в якому потрібно здійснити заміну, `from` - рядок, який потрібно замінити, та `to` - рядок, на який необхідно замінити `from`. Наприклад, якщо у нас є рядок `Hello, world!`, і ми хочемо замінити слово `world` на `Ukraine`, код буде виглядати так:

```C
char *new_string = str_replace("Hello, world!", "world", "Ukraine");
```

Після цього вміст змінної `new_string` буде `Hello, Ukraine!`. 

## Глибоке занурення

Функція `str_replace()` також має можливість здійснювати заміни у великих текстових файлів. Для цього необхідно використати комбінацію функцій `fgets()` та `fprintf()`. Приклад коду для заміни першої входження слова `apple` на `banana` у великому текстовому файлі `file.txt` виглядатиме так:

```C
FILE *file = fopen("file.txt", "r");
FILE *new_file = fopen("new_file.txt", "w");

char line[100];
while (fgets(line, 100, file)) {
    char *new_line = str_replace(line, "apple", "banana");
    fprintf(new_file, "%s", new_line);
}

fclose(file);
fclose(new_file);
```

У цьому прикладі ми використовуємо `fgets()` для зчитування рядка з файлу `file.txt`, замінюємо у ньому слово `apple` на `banana` за допомогою функції `str_replace()` і записуємо змінений рядок у новий файл `new_file.txt` за допомогою `fprintf()`.

## Дивись також

1. [Функція str_replace() у мові програмування PHP](https://www.php.net/manual/ru/function.str-replace.php)
2. [Розробка великих проектів у мові C](https://habr.com/ru/post/422989/)
3. [Підручник з мови C для початківців](https://proglib.io/p/c-for-beginners/)