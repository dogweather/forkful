---
title:                "Надсилання http-запиту"
html_title:           "C#: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

Що і чому?

Надсилання HTTP-запиту є важливою частиною розробки програм. Це дозволяє програмі взаємодіяти з сервером, отримувати та передавати дані. Без надсилання HTTP-запитів програми не змогли би працювати з інтернет-ресурсами, такими як веб-сторінки та додатки.

Як це зробити:

Використовуючи мову програмування C#, надсилання HTTP-запиту стає дуже простим. Ось приклад коду, який надсилає GET-запит до веб-сайту "example.com" і виводить вміст сторінки у консоль:

```C#
using System;
using System.Net.Http;

public class Program {
    public static async Task Main() {
        using (var client = new HttpClient()) {
            var result = await client.GetAsync("http://example.com");
            string content = await result.Content.ReadAsStringAsync();
            Console.WriteLine(content);
        }
    }
}
```

Результат виконання цього коду виведе вміст сторінки "example.com" у консоль:

```html
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>

<body>
<div>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents. You may use this
    domain in literature without prior coordination or asking for permission.</p>
</div>
</body>
</html>
```

Глибоке погруження:

Надсилання HTTP-запитів стало можливим завдяки протоколу HTTP (Hypertext Transfer Protocol), який був розроблений у 1990-х роках. Існують інші альтернативи взаємодії з серверами, наприклад, FTP-протокол для передачі файлів чи SMTP-протокол для електронної пошти. Однак, HTTP є стандартним для надсилання запитів на веб-сайти та інші інтернет-ресурси.

Як ви могли помітити, ми використовували клас HttpClient для надсилання нашого запиту. Цей клас є частиною простору імен System.Net.Http, який надає можливість взаємодіяти з веб-серверами за допомогою HTTP. Також існують інші бібліотеки, такі як RestSharp, які дозволяють більш гнучко взаємодіяти з веб-серверами та обробляти відповіді.

Також, HTTP-запити мають різні методи, такі як GET, POST, PUT, DELETE, які використовуються для звернення до різного типу ресурсів та виконання дій над ними. Для детальнішої інформації про роботу з HTTP-запитами та їх використання, рекомендуємо ознайомитися з документацією Microsoft.

Рекомендовані джерела:

- [Документація Microsoft про надсилання HTTP-запитів](https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/console-webapiclient)
- [RestSharp - бібліотека для взаємодії з веб-серверами за допомогою REST API](https://restsharp.dev/)
- [Протокол HTTP на Вікіпедії](https://uk.wikipedia.org/wiki/HTTP)