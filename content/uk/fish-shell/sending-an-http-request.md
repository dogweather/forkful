---
title:                "Відправлення http-запиту"
html_title:           "Fish Shell: Відправлення http-запиту"
simple_title:         "Відправлення http-запиту"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому

Надсилання HTTP запиту є важливою частиною розробки програмного забезпечення. Це дозволяє отримати дані з сервера та взаємодіяти з веб-сайтами.

## Як виконати

Надсилання HTTP запиту в Fish Shell можливо за допомогою команди `curl`. Нижче наведено приклад коду та очікуваний результат:

```Fish Shell
curl https://www.example.com
<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
...
</head>
<body>
<h1>Example Domain</h1>
<p>This domain is for use in illustrative examples in documents. ...</p>
</body>
</html>
```

## Поглиблене дослідження

Команда `curl` виглядає просто, але насправді це потужний інструмент з багатьма опціями. Для отримання більшої інформації можна використати допомогу команди `curl` або прочитати документацію.

## Дивись також

- [Документація з команди `curl`](https://curl.se/docs/manual.html)
- [Допомога по команді `curl` в Fish Shell](https://fishshell.com/docs/current/commands.html#curl)