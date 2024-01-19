---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Читання текстового файлу - це процес отримання даних з файлу і їх використання у програмних додатках. Програмісти роблять це, щоб зберегти, аналізувати та маніпулювати великими обсягами даних з легкістю і ефективністю.

## Як це робити:

Код та приклад виводу вуглецю:

```PHP
<?php
$file = 'example.txt';

if (file_exists($file)){
    $content = file_get_contents($file);
    echo $content;
} else {
    echo "$file не існує";
}
?>
```
Цей код прочитає та виведе вміст файлу "example.txt". Якщо файлу не існує, ви отримаєте повідомлення, що "$file не існує".

## Глибше занурення:

Історія: зазвичай в PHP для зчитування текстових файлів використовували функції `fopen`, `fread`, `fclose`. Однак, `file_get_contents` стала більш простою та зручною альтернативою.
Альтернативи: крім `file_get_contents`, можна використовувати `fopen` з `fread` і `fclose`. Цей метод більш гнучкий, але потребує більшого коду.
Деталі реалізації: `file_get_contents` працює швидше з великими файлами, адже вона читає весь файл в одну строку. З іншого боку, `fopen` та `fread` читають файл по частинах, що походить для контролю пам'яті.

## Див. також:

- [Офіційна документація PHP по file_get_contents](https://www.php.net/manual/en/function.file-get-contents.php)
- [Офіційна документація PHP по fopen](https://www.php.net/manual/en/function.fopen.php)