---
title:                "Надсилання http-запиту з базовою аутентифікацією"
html_title:           "Python: Надсилання http-запиту з базовою аутентифікацією"
simple_title:         "Надсилання http-запиту з базовою аутентифікацією"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Що і чому?
Надсилання HTTP-запиту з основною аутентифікацією - це процес надання ідентифікаційних даних для перевірки автентичності при здійсненні запиту до веб-сайту чи веб-служби. Програмісти використовують цей метод для забезпечення безпечного обміну даними між користувачем та веб-сайтом чи службою.

Як це зробити:
```Python
import requests
from requests.auth import HTTPBasicAuth

url = "http://example.com/api"
username = "username"
password = "password"

response = requests.get(url, auth=HTTPBasicAuth(username, password))

print(response.status_code)
print(response.text)
```

Виводиться:

200
Hello, world!

Глибоке дослідження:
1. Історичний контекст: Основна аутентифікація була стандартизована у 1996 році відповідно до RFC 2617.
2. Альтернативи: Альтернативними методами аутентифікації є базова аутентифікація з використанням токенів та базова аутентифікація з використанням особистого ключа.
3. Деталі реалізації: Логін та пароль пересилаються у заголовку HTTP-запиту в форматі Base64. Однак, цей метод не є надійним, тому слід застосовувати й інші методи безпеки, наприклад, HTTPS.

Дивіться також:
Подробиці щодо HTTP-запитів та аутентифікації можна знайти у документації requests (https://requests.readthedocs.io/en/master/) та RFC 2617 (https://tools.ietf.org/html/rfc2617).