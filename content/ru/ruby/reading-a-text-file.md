---
title:                "Чтение текстового файла"
date:                  2024-01-29T00:01:07.400053-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение текстового файла"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Чтение текстового файла означает доступ к содержимому файла, сохраненному на диске, через код. Программисты делают это для обработки, анализа или отображения данных в своих приложениях.

## Как:

Чтение файла в Ruby просто. Можно использовать класс `File`, который предоставляет различные методы чтения файлов. Вот простой пример чтения всего файла:

```Ruby
File.open("example.txt", "r") do |file|
  puts file.read
end
```

Если `example.txt` содержит текст "Привет, Ruby!", вот что вы получите:

```
Привет, Ruby!
```

Для чтения построчно:

```Ruby
File.foreach("example.txt") { |line| puts line }
```

Тот же `example.txt`, теперь вывод будет построчный:

```
Привет, Ruby!
```

## Глубокое погружение:

Исторически чтение файлов было ключевой возможностью языков программирования, позволяющих взаимодействовать с файловой системой.

В Ruby вы также можете читать файл с помощью разных инструментов:

1. Класс `IO`: Для низкоуровневых файловых операций.
2. Метод `readlines`: Загружает весь файл в массив, причем каждая строка является элементом.
3. `File.read`: Быстрый способ прочитать весь файл в строку.

Следует учитывать компромисс: `File.read` удобен для маленьких файлов, но может быть ресурсоемким для больших. Вот тогда чтение построчно или частями становится ценным.

## Смотрите также:

- Документация Ruby для класса `File`: [ruby-doc.org/core/File.html](https://ruby-doc.org/core/File.html)
- Обсуждения на Stack Overflow о чтении файлов в Ruby: [stackoverflow.com/questions/tagged/ruby+file-io](https://stackoverflow.com/questions/tagged/ruby+file-io)