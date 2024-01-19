---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Надсилання HTTP-запиту - це процес надсилання запиту від клієнта до сервера через протокол HTTP. Програмісти роблять це для отримання або відправлення даних на веб-сервер або API.

## Як це зробити:

```Fish Shell
# Встановлення програми curl
sudo apt-get install curl

# Надсилання GET-запиту
curl "http://example.com"

# Надсилання POST-запиту з JSON-даними
curl -d '{"key1":"value1", "key2":"value2"}' -H "Content-Type: application/json" -X POST http://localhost:3000/data
```

**Вивід:**
```Fish Shell
<html>
<head>
<title>Example Domain</title>
...
</html>
```

## Поглиблений огляд:

(1) Історичний контекст: HTTP-запити були винайдені в 1991 році як основний метод передачі даних в World Wide Web.
(2) Альтернативи: Деякі альтернативи HTTP-запитів включають WebSocket або документи GraphQL.
(3) Деталі реалізації: У Fish Shell можна використовувати утиліту curl або wget для відправлення HTTP-запитів. Ви можете налаштувати деталі запиту, такі як заголовки, методи (GET, POST, DELETE тощо) і дані запиту.

## Дивись також:

- Документація про HTTP-запити: [ссылка](https://developer.mozilla.org/uk/docs/Web/HTTP/Overview)
- Fish Shell scripting tutorial: [ссылка](https://fishshell.com/docs/current/tutorial.html)
- Curl manual: [ссылка](https://curl.se/docs/manual.html)