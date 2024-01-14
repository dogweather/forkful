---
title:                "Bash: Аналізування html"
simple_title:         "Аналізування html"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

Розбір HTML - це процес, який дозволяє витягувати специфічну інформацію з веб-сторінок. Це може бути корисно для отримання даних зі сторінок, які не мають простого API або для веб-скрапінгу.

## Як це зробити

Для розбирання HTML у Bash використовуються утиліти, такі як `grep` та `sed`. Нижче наведений приклад коду, який витягує всі посилання на зображення з веб-сторінки:

```Bash
#!/bin/bash

# Виконати запит і зберегти в результат у змінну
res=$(curl -sS https://example.com)

# Використати grep для видалення всіх рядків, що не містять посилання на зображення
imgs=($(echo "$res" | grep -Eo "https?://[^>\"]+?\.(jpg|png|gif)"))

# Вивести результат
for img in "${imgs[@]}"; do
  echo "$img"
done
```

Вихідний код буде виглядати приблизно так:

```
https://example.com/img1.jpg
https://example.com/img2.png
https://example.com/img3.gif
```

Цей приклад демонструє, як легко можна використовувати утиліти для розбирання HTML у Bash.

## Глибока затонув

Утиліти `grep` та `sed` можуть виконувати багато різних завдань з розбирання HTML. Наприклад, ви можете витягувати певні теги чи атрибути з веб-сторінки. Також є спеціальні утиліти, такі як `html2text`, які дозволяють конвертувати HTML у звичайний текст для подальшої обробки.

Є також більш потужні інструменти для розбирання HTML, такі як `awk` та `perl`, які дозволяють більш гнучко виконувати завдання. Але `grep` та `sed` будуть достатньо для більшості простих задач.

## Дивіться також

- [Розбирання HTML за допомогою Bash](https://www.linuxjournal.com/content/bash-html-processing-made-easy)
- [HTML утиліти для Bash](https://www.scriptinglibrary.com/html-utilities-for-bash/) 
- [Використання grep та sed для розбирання HTML](https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags)