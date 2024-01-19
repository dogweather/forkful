---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо це?

Отже, відправка HTTP-запиту із базовою автентифікацією – це процес передачі даних на веб-сервер з використанням методу автентифікації. Програмісти роблять це, щоб обмінюватися данними в безпечному середовищі.

## Як це зробити:

Базовий приклад: 

```Bash
username='username'
password='password'
auth=$(echo -n "$username:$password" | base64)
curl -H "Authorization: Basic $auth" http://somewebsite.com
```

Якщо ви запустите цей скрипт, ви потенційно отримаєте відгук від `http://somewebsite.com`.

## Поглиблено

1. **Історичний контекст**: Метод базової автентифікації був стандартом HTTP/1.0, проте із часом його стали заміщувати складніші системи автентифікації.
2. **Альтернативи**: Токени `Bearer` часто використовуються як альтернатива для автентифікації, оскільки вони надають більше безпеки і можливостей для контролю.
3. **Деталі реалізації**: Використання `base64` для кодування в базовому методі автентифікації HTTP не є методом шифрування; це просто кодування. Це важливо розуміти, тому що `base64` можна легко розкодувати.

## Див. також

[HTTP authentication](https://en.wikipedia.org/wiki/Basic_access_authentication) - Wikipedia стаття про базову автентифікацію HTTP.

[Bearer token](https://oauth.net/2/bearer-tokens/) - інформація про токени та їх використання для автентифікації. 

[cURL manual](https://curl.se/docs/manpage.html) - деталі про використання cURL в командному рядку.