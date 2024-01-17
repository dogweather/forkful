---
title:                "Перетворення рядка на заголовні літери"
html_title:           "Fish Shell: Перетворення рядка на заголовні літери"
simple_title:         "Перетворення рядка на заголовні літери"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що це таке і для чого?
Капіталізація рядка - це процес перетворення першої літери рядка на велику. Це широко використовується в програмуванні, особливо при роботі з текстом, для полегшення читання і розрізнення різних рядків.

## Як це зробити:
Код і приклади використання капіталізації рядка у Shell Fish (актуальна версія):

```Fish Shell
set my_string 'hello world'
echo $my_string # виведе 'hello world'

echo (string match -r '(\w+)' $my_string | string replace -r --uppercase '$1')

# виведе 'Hello World'
```

## Поглиблене дослідження:
 - Капіталізація рядка має свої коріння у давній історії друкування, коли кожне слово починалося з великої літери для підвищення читабельності.
 - Існують інші способи для капіталізації рядків, наприклад, використання функцій програмування, але використання Shell Fish - це швидкий і зручний спосіб для цього.
 - У Shell Fish, капіталізація рядка реалізована за допомогою вбудованих функцій `string match` та `string replace`.

## Дивись також:
Посилання на додаткові інформаційні джерела:
 - [Fish Shell документація](https://fishshell.com/docs/current/)
 - [Стаття про капіталізацію рядка на GeekyMentor](https://geekymentor.com/string-capitalization-in-java/)
 - [Приклади використання Shell Fish на GitHub](https://github.com/fish-shell/fish-shell/wiki/Cookbook-:-Useful-snippets-and-functions)