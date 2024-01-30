---
title:                "Поиск длины строки"
date:                  2024-01-28T23:58:44.799218-07:00
model:                 gpt-4-0125-preview
simple_title:         "Поиск длины строки"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/gleam/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Нахождение длины строки означает подсчет количества символов, которые она содержит. Программисты делают это для валидации ввода, разделения строк или просто чтобы узнать, насколько "велик" данный текст.

## Как это сделать:

В Gleam вычислить длину строки можно в одну строку кода. Вот как это делается:

```Gleam
import gleam/io
import gleam/string

fn main() {
  let my_string = "Gleam shines!"
  let length = string.len(my_string)
  io.println(length) // Вывод: 12
}
```

Просто используйте функцию `string.len` и передайте в нее вашу строку. Бум! Вы получили ее длину.

## Глубокое погружение

Когда-то строки были похожи на магические заклинания — их было сложно обрабатывать, и каждый язык применял свои собственные заклинания. В языках, таких как C, вам нужно было вручную пройтись по массиву символов до тех пор, пока вы не наткнетесь на нулевой терминатор (`'\0'`), чтобы найти длину строки. Больно, правда?

Напротив, Gleam держит все просто. Он работает на BEAM VM — дом для Erlang и Elixir, которые рассматривают строки как последовательность байтов. Верно, байтов, а не символов. Это ключевой момент, потому что в Unicode символы могут занимать более одного байта. Строки в Gleam закодированы в UTF-8, поэтому один символ может занимать от 1 до 4 байтов.

Вот засада — `string.len` показывает вам количество байтов, а не количество кластеров Unicode графем (то, что мы часто считаем символами). Таким образом, для строк ASCII (где каждый символ — это один байт), длина в байтах совпадает с количеством символов. Для строк, содержащих эмодзи или другие многобайтовые символы, это уже не так.

Для быстрого решения в Gleam прямо сейчас нет встроенной альтернативы. Вам нужно будет подключить библиотеку или написать немного кода самостоятельно, если вам нужно подсчитать кластеры графем.

## Смотрите также

Подробнее изучите обработку строк в Gleam в официальной документации:


И для наглядного представления графем, байтов и символов, посмотрите:

- Просмотрщик кластеров графем Unicode: [https://util.unicode.org/UnicodeJsps/list-unicodeset.jsp?a=%5B%3AGrapheme_Cluster_Break%3DControl%3A%5D&abb=on&esc=on&g=&i=](https://util.unicode.org/UnicodeJsps/list-unicodeset.jsp?a=%5B%3AGrapheme_Cluster_Break%3DControl%3A%5D&abb=on&esc=on&g=&i=)