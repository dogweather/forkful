---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і чому?

Отправлення HTTP-запиту з базовою аутентифікацією - це спосіб передачі облікових даних користувача на сервер. Пристосовуємо це для того, щоб забезпечити безпечний доступ до ресурсів, здебільшого веб-сервісів.

## Як це зробити:

Спробуйте наступний код:

```C#
using System;
using System.Net;
using System.Text;

var username = "yourUsername";
var password = "yourPassword";
var httpWebRequest = (HttpWebRequest)WebRequest.Create("http://yoururl.com");
httpWebRequest.Headers["Authorization"] = "Basic " + Convert.ToBase64String(Encoding.Default.GetBytes(username + ":" + password));
httpWebRequest.Method = "GET";

var httpResponse = (HttpWebResponse)httpWebRequest.GetResponse();
using (var streamReader = new StreamReader(httpResponse.GetResponseStream()))
{
    var result = streamReader.ReadToEnd();
}
```

Цей код створює HTTP-запит, додає заголовок для базової аутентифікації і відправляє запит.

## Поглиблений розбір: 

1. **Історичний контекст**: Базова аутентифікація HTTP була стандартом з часів специфікації HTTP/1.0, хоча незахищена і рекомендована тільки для захищених підключень.
2. **Альтернативи**: Сучасніші методи обіймуть Токени OAuth, Cookie, або JWT (JSON Web Tokens).
3. **Деталі реалізації**: Наш метод реалізації передачі облікових даних в заголовку, "Basic ", після чого слідує користувач та пароль, кодовані в base64.

## Дивіться також:

1. [Докладніше про HTTP-аутентифікацію](https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication)
2. [Токени OAuth](https://oauth.net/)
3. [JWT (JSON Web Tokens)](https://jwt.io/)