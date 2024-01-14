---
title:    "Gleam: Читання текстового файлу"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Чому

Зчитування текстового файлу є важливою навичкою для програмістів у будь-якому мові програмування. Це дозволяє отримати потрібні дані з файлу і використовувати їх у своїх програмах.

## Як це зробити

Для зчитування текстового файлу в Gleam використовується функція `File.read` із модулю `gleam/io`. Для початку, необхідно вимагати цей модуль:

```Gleam
import gleam/io
```

Потім, можна використати функцію `File.read` з назвою файлу як аргументом:

```Gleam
let file_content = File.read("file.txt")
```

Це зчитає вміст файлу з назвою `file.txt` і збереже його у змінну `file_content`. Далі, можна використати цей вміст у своїй програмі.

Існує також багато інших методів для зчитування файлів в Gleam, таких як `File.read_to_string` для зчитування текстового файлу як рядка, або `File.read_lines` для зчитування файлу рядками.

## Поглиблене вивчення

Зчитування текстового файлу може бути корисною навичкою, але також може бути дещо складною. Наприклад, якщо файл є дуже великим, це може призвести до помилки за нестачі пам'яті. Тому важливо враховувати цей аспект, коли пишете код для зчитування файлів.

Для докладнішої інформації про зчитування файлів в Gleam, можна ознайомитися з [офіційною документацією](https://gleam.run/documentation/io.html) або прикладами кодів у [репозиторії з прикладами](https://github.com/gleam-lang/examples/tree/master/io).

## Дивіться також

- [Офіційна документація з `gleam/io`](https://gleam.run/documentation/io.html)
- [Приклади кодів у репозиторії з прикладами](https://github.com/gleam-lang/examples/tree/master/io)