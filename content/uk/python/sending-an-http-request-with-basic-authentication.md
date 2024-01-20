---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і чому?

Відправлення HTTP запиту з базовою аутентифікацією - це процес, коли ваша програма надсилає серверу запит HTTP, використовуючи основні облікові дані для аутентифікації. Це потрібно для безпечної взаємодії з веб-серверами, що вимагають авторизації.

## Як це зробити:

Ми використовуватимемо модуль requests у Python. Ось приклад коду:

```Python
import requests
from requests.auth import HTTPBasicAuth

response = requests.get('https://api.github.com/user', auth=HTTPBasicAuth('user', 'pass'))
print(response.status_code)
```

Якщо ви виконаєте цей код з вірними обліковими даними, відповідь буде 200, що свідчить про успішну аутентифікацію.

## Поглиблено:

**Історичний контекст**: Базова аутентифікація HTTP була однією з перших методів аутентифікації, використаних в HTTP. Вона передбачає відправлення імені користувача та пароля в незашифрованому вигляді в заголовках запитів.

**Альтернативи**: Хоча базова аутентифікація проста в використанні, вона не надає достатнього захисту для більшості сучасних застосунків. Альтернативи, такі як OAuth або Digest Authentication, є безпечнішими варіантами.

**Подробиці реалізації**: Модуль requests у Python автоматично кодує ваші облікові дані в Base64 і вставляє їх у заголовок `Authorization` вашого HTTP запиту.

## Див. також:

Модуль requests Python: https://docs.python-requests.org/en/latest/
Аутентифікація HTTP в MDN web docs: https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication 
Безпечність HTTP: https://owasp.org/www-community/OWASP_Inssecure_Configuration_Guide