---
title:                "Надсилання запиту http з базовою аутентифікацією"
html_title:           "Python: Надсилання запиту http з базовою аутентифікацією"
simple_title:         "Надсилання запиту http з базовою аутентифікацією"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Зачем
Базова автентифікація HTTP використовується для захищеного доступу до ресурсів в інтернеті. Вона дозволяє аспектам безпеки, таким як ідентифікація та авторизація, контролювати доступ до веб-сторінок та додатків.

## Як
```Python
import requests
from requests.auth import HTTPBasicAuth

# Введіть базову URL та свої дані для аутентифікації
url = 'https://example.com/api/users'
username = 'user123'
password = 'password123'

# Отримайте доступ до веб-ресурсів за допомогою базової автентифікації
response = requests.get(url, auth=HTTPBasicAuth(username, password))

# Перевірте статус коду відповіді
if response.status_code == 200:
    print("Запит було успішно опрацьовано")
elif response.status_code == 401:
    print("Неправильні дані для аутентифікації")
```
Вивід:
```
Запит було успішно опрацьовано
```

## Глибше
Крім базової автентифікації, існує ще й декілька інших методів аутентифікації HTTP, таких як Digest та OAuth. Однак, базова автентифікація відмінно працює для простих веб-додатків та має простий механізм безпеки - передача незашифрованого пароля по мережі відбувається за допомогою кодування base64.

# Дивись Також
- [Офіційна документація Python для бібліотеки Requests](https://requests.readthedocs.io/en/master/user/authentication/)
- [Стаття про різні методи аутентифікації HTTP](https://www.geeksforgeeks.org/http-authentication/)
- [Відеоурок про базову автентифікацію HTTP в Python](https://www.youtube.com/watch?v=1SmgKzRezXY)