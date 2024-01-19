---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що і чому?
Завантаження веб-сторінки - це процес отримання даних з сервера і їхнього збереження на локальному пристрої. Програмісти роблять це, щоб аналізувати, тестувати або зберігати вміст веб-сторінки.

## Як це зробити:
```Fish Shell
function download-webpage
    set url $argv[1]
    set output $argv[2]
    
    curl $url -o $output
end

# Використання
download-webpage https://example.com output.html
```
При запуску цієї програми, вона завантажить веб-сторінку `https://example.com` і збереже її у файл `output.html`.

## Поглиблений розділ
Завантаження веб-сторінок є основою інтернету і датується початками WWW в 1991 році. Ви також можете використовувати інструменти, такі як "wget" чи "httpie" для виконання схожих завдань. Програма вище використовує "curl", потужний інструмент для роботи з URL, як елементарну HTTP-клієнтську імплементацію.

## Додаткова інформація
Перегляньте наступні посилання, щоб дізнатися більше з цієї теми:

+ Основи HTTP: https://developer.mozilla.org/uk/docs/Web/HTTP/Overview
+ Документація по `curl`: https://curl.se/docs/manpage.html
+ Деталі HTTP-клієнтів на Fish Shell: https://fishshell.com/docs/current/commands.html#commands