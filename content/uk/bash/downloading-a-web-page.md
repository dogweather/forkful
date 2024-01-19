---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що і чому?

Скачування веб-сторінки означає отримання її вмісту та зберігання його на вашому комп’ютері. Програмісти цим займаються, щоб аналізувати її вміст або щоб використати її офлайн.

## Як це зробити:

За допомогою Bash, ми можемо використовувати команду `curl` або `wget` для скачування веб-сторінок. Ось приклади:

```Bash
# Користуючись curl
curl http://example.com -o example.html

# Користуючись wget
wget http://example.com
```
Використовуючи команди вище, ми скачуємо веб-сторінку `example.com` і зберігаємо її у файл `example.html`.

## Пірнення вглиб

1. **Історичний контекст:** `Curl` та `wget` були створені у 90-х з метою скачування вмісту з Інтернету через командний рядок.

2. **Альтернативи:** Існують альтернативи, такі як `lynx` і `links`, але `curl` та `wget` є найпопулярнішими.

3. **Деталі реалізації:** команда `curl` завантажує веб-сторінку і передає її STDOUT, тоді як `wget` автоматично зберігає вміст в файл.

## Дивіться також

1. [Curl документація](https://curl.se)
2. [Wget документація](https://www.gnu.org/software/wget/)
3. [Lynx документація](http://lynx.browser.org/)
4. [Links документація](http://links.twibright.com/user_en.html)