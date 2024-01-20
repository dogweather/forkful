---
title:                "Надсилання http-запиту з базовою аутентифікацією"
html_title:           "Arduino: Надсилання http-запиту з базовою аутентифікацією"
simple_title:         "Надсилання http-запиту з базовою аутентифікацією"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Відправлення HTTP-запиту з базовою аутентифікацією — це процес передачі користувацького ім'я та пароля до веб-серверу для отримання доступу до приватного ресурсу. Програмісти це роблять, щоб захистити конфіденційні дані від несанкціонованого доступу.

## Як це зробити:

В Lua для відправлення HTTP-запитів ми можемо використовувати бібліотеку `socket.http`.
```Lua
http = require('socket.http')
http.USERAGENT = "Mozilla/5.0"

function basicAuth(user, password)
  local auth = "Basic " .. (user .. ":" .. password):base64Encode()
  return {authorization = auth}
end

local r, c, h = http.request{
  url = 'http://example.com',
  headers = basicAuth('your_username', 'your_password')
}
```
Очікуваний вивід: 
```Lua
print(r) -- поверне вміст веб-сторінки, або nil, якщо сталася помилка
print(c) -- повертає HTTP-статус
```
## Більше деталей:

Базова аутентифікація є однією з перших методів аутентифікації, впроваджених в стандарт HTTP. Має бути використано із HTTPS, бо авторизаційні дані відправляються в незашифрованому вигляді.

Альтернативи базовій аутентифікації включають Digest Authentication та OAuth.

Метод `http.request` в бібліотеці `socket.http` підтримує кастомні заголовки, що дозволяє встановити заголовок Authorization, а занчення "Basic " + base64Encode(username + ":" + password).

## Додатково:

1. Розділ про аутентифікацію в документації HTTP: https://tools.ietf.org/html/rfc7235
2. LuaSocket HTTP manual: https://w3.impa.br/~diego/software/luasocket/http.html
3. Модуль base64 для Lua: https://github.com/ErnieE5/luajson/blob/master/lua/base64.lua