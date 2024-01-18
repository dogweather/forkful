---
title:                "Надсилання запиту http з основною автентифікацією"
html_title:           "Lua: Надсилання запиту http з основною автентифікацією"
simple_title:         "Надсилання запиту http з основною автентифікацією"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# що і чому?
Відправлення HTTP запиту з основною автентифікацією - це один з способів, яким програмісти можуть обмінюватися даними з веб-сервером за допомогою програмного забезпечення. Цей метод забезпечує безпеку передачі даних шляхом перевірки ідентифікатора та пароля.

# Як:
```Lua
local http = require("socket.http")
local b64 = require("base64")  -- вимагає інсталяції лише у випадку Lua 5.1

-- первинні дані для запиту
local url = "http://example.com/api/data"
local username = "myusername"
local password = "mypassword"

-- створення заголовків запиту з основною автентифікацією
local headers = { Authorization = "Basic " .. b64.enc(username .. ":" .. password) }

-- відправлення запиту з мінімальним таймаутом виконання та отримання відповіді
local response, status = http.request(url, headers)

-- перевірка коду відповіді (200 означає успішну автентифікацію)
if status == 200 then
  -- робота з отриманими даними
  print(response)

else
  -- обробка помилки
  print("Помилка при автентифікації: " .. status)
end
```

# Поглиблене вивчення:
На початку створення інтернету, передача даних відбувалась без будь-яких заходів безпеки, що призвело до багатьох вразливостей. Основна автентифікація, заснована на протоколі HTTP, стала першою спробою забезпечити безпеку при обміні даними. Однак вважається, що цей метод не є надійним і існують кращі альтернативи, такі як OAuth або OpenID. Імплементація основної автентифікації в Lua використовує пакет base64 для кодування ідентифікатора та пароля у форматі, який може бути переданий через заголовок HTTP запиту.

# Дивіться також:
- [Документація по HTTP запитах з socket.http](https://w3.impa.br/~diego/software/luasocket/http.html)
- [Стандарт Lua для базового кодування і декодування Base64](https://www.lua.org/manual/5.3/manual.html#6.7)
- [Детальна стаття з оглядом основної автентифікації](https://dev.opera.com/articles/http-basic-authentication/)