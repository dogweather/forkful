---
title:                "Завантаження веб-сторінки"
html_title:           "Javascript: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Чому і для чого?

Завантаження сторінки в Інтернеті означає отримання всієї інформації, яка є на даній сторінці, замість того, щоб переглядати готовий вигляд в браузері. Програмісти часто завантажують сторінки для отримання потрібних даних для своїх проектів.

Як це зробити:

```Javascript
fetch('https://www.example.com')
  .then(response => response.text())
  .then(data => console.log(data))
  .catch(err => console.log(err));
```

Результат:

```HTML
<!DOCTYPE html>
<html>
<head>
  <title>Example Domain</title>
  <meta charset="utf-8" />
</head>
<body>
<div><h1>Example Domain</h1></div>
<div>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</div>
<div><a href="https://www.iana.org/domains/example">More information...</a></div>
</body>
</html>
```

Глибоке занурення:

Зазвичай, завантаження веб-сторінок використовується для отримання даних для подальшого аналізу або використання в проекті. Існують інші методи, такі як парсинг HTML або використання спеціальних бібліотек, але завантаження сторінки за допомогою Javascript є найшвидшим і простим способом отримання необхідних даних. Також, важливо зазначити, що завантаження сторінки необхідно виконувати відповідно до законів про авторські права і з урахуванням прав доступу до веб-сторінок.

Подивіться також:

- [Fetch API](https://developer.mozilla.org/uk/docs/Web/API/Fetch_API)
- [Axios](https://axios-http.com/)
- [Web Scraping](https://www.geeksforgeeks.org/what-is-web-scraping-and-how-to-extract-useful-information-from-the-web/)