---
title:                "Чтение текстового файла"
date:                  2024-01-29T00:01:01.355309-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение текстового файла"

category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/php/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Чтение текстового файла в PHP означает извлечение содержимого файла в ваш скрипт. Программисты делают это для обработки хранения данных, конфигурации или для обработки больших наборов данных без загромождения своего кода.

## Как это сделать:
### Используя `file_get_contents`:
```PHP
$content = file_get_contents("example.txt");
echo $content;
```
Пример вывода:
```
Привет, Мир!
Это содержимое из текстового файла.
```

### Используя `fopen` и `fgets`:
```PHP
$handle = fopen("example.txt", "r");
if ($handle) {
    while (($line = fgets($handle)) !== false) {
        echo $line;
    }
    fclose($handle);
}
```
Пример вывода:
```
Привет, Мир!
Это содержимое из текстового файла.
```

### Запись в файл с помощью `file_put_contents`:
```PHP
$newContent = "Добавление нового текста.";
file_put_contents("example.txt", $newContent);
```

## Глубокое погружение
Чтение текстовых файлов — это столько же лет, сколько и само программирование. До появления баз данных, конфигурационные файлы и пользовательские данные часто хранились в простых текстовых файлах. Альтернативы, такие как XML и JSON файлы, структурированны, легче разбираются и хорошо подходят для сложных данных.

В PHP, `file_get_contents` и `file()` быстры для чтения; первый получает все в одной строке, а второй в массиве. `fopen` в сочетании с `fgets` или `fread` дает вам больше контроля, особенно для больших файлов, так как вы читаете его построчно или блоками.

Некоторые нюансы: `fopen` требует соответствующих разрешений, иначе он не удастся; обработка его ошибок считается лучшей практикой. При использовании `file_put_contents` имейте в виду, что по умолчанию он перезаписывает файл; используйте флаг `FILE_APPEND`, чтобы добавить содержимое вместо этого.

## См. также
- Справочник PHP по `file_get_contents`: https://www.php.net/manual/ru/function.file-get-contents.php
- Справочник PHP по `fopen`: https://www.php.net/manual/ru/function.fopen.php
- Справочник PHP по `fgets`: https://www.php.net/manual/ru/function.fgets.php
- Справочник PHP по `file_put_contents`: https://www.php.net/manual/ru/function.file-put-contents.php
- Руководство по работе с файлами PHP: https://www.w3schools.com/php/php_file.asp
