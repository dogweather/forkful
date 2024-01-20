---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?

HTTP-запит - це засіб, яким програми на вашому комп'ютері звертаються до серверів в Інтернеті, щоб отримати або відправити інформацію. Програмісти використовують їх, щоб інтерактивно працювати з веб-ресурсами.

## Як це зробити:

Lua допомагає нам виконувати HTTP-запити з допомогою модулів, таких як `luasocket` і `luasec`. 

У випадку з GET-запитом:
```lua
http = require('socket.http')
url = "http://example.com"
response, status = http.request(url)
print(response)
```

Вивід:
```
<!doctype html>...
```

Для POST-запиту:
```lua
http = require('socket.http')
url = "http://example.com/form_submit"
body = "name=John%20Doe&email=john%40example.com"
response, status = http.request(url, body)
print(response)
```
Зверніть увагу, що тіло повинно бути в форматі `application/x-www-form-urlencoded` для POST-запиту.

## Поглиблений аналіз

Lua не була заснована з можливістю відправляти HTTP-запити "з коробки". Ця функція була додана пізніше через зросле потреби програмістів. Альтернативи `luasocket` і `luasec` включають більш сучасні модулі, такі як `lua-http` або `lua-resty-http`.

HTTP-запити - це ключовий компонент розробки веб-застосунків, але їх застосування далеко не обмежується цим. Вони використовуються для віддаленого керування, отримання даних в реальному часі, роботи з API та інше.

## Додатково

- Стандарт HTTP: https://www.ietf.org/standards/rfcs/
- Модуль `luasocket`: https://github.com/diegonehab/luasocket
- Модуль `lua-http`: https://github.com/daurnimator/lua-http/
- Модуль `lua-resty-http`: https://github.com/ledgetech/lua-resty-http