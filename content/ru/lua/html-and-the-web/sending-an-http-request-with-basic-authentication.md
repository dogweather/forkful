---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:22.055757-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Lua \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\
  \u043D\u043D\u043E\u0439 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0438\
  \ HTTP, \u043F\u043E\u044D\u0442\u043E\u043C\u0443 \u0432\u0430\u043C \u043F\u043E\
  \u043D\u0430\u0434\u043E\u0431\u0438\u0442\u0441\u044F \u0432\u043D\u0435\u0448\u043D\
  \u044F\u044F \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430, \u043D\
  \u0430\u043F\u0440\u0438\u043C\u0435\u0440, `socket.http` \u0438\u0437 LuaSocket\
  \ \u0438\u043B\u0438 `http.request`\u2026"
lastmod: '2024-03-13T22:44:45.287934-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Lua \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\
  \u043D\u043E\u0439 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0438 HTTP,\
  \ \u043F\u043E\u044D\u0442\u043E\u043C\u0443 \u0432\u0430\u043C \u043F\u043E\u043D\
  \u0430\u0434\u043E\u0431\u0438\u0442\u0441\u044F \u0432\u043D\u0435\u0448\u043D\u044F\
  \u044F \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430, \u043D\u0430\
  \u043F\u0440\u0438\u043C\u0435\u0440, `socket.http` \u0438\u0437 LuaSocket \u0438\
  \u043B\u0438 `http.request` \u0438\u0437 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\
  \u0435\u043A\u0438 `http`, \u0435\u0441\u043B\u0438 \u0432\u044B \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u0443\u0435\u0442\u0435 Lua 5.3+."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
weight: 45
---

## Как это сделать:
В Lua нет встроенной поддержки HTTP, поэтому вам понадобится внешняя библиотека, например, `socket.http` из LuaSocket или `http.request` из библиотеки `http`, если вы используете Lua 5.3+. Для базовой аутентификации закодируйте учетные данные и добавьте их в заголовок запроса.

```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")
local mime = require("mime")

-- Ваши учетные данные
local username = "Aladdin"
local password = "openSesame"
local credentials = mime.b64(username .. ":" .. password)

-- Настройка запроса
local response_body = {}
local res, code, response_headers = http.request{
    url = "http://example.com/data",
    method = "GET",
    headers = {
        ["Authorization"] = "Basic " .. credentials
    },
    sink = ltn12.sink.table(response_body)
}

-- Вывод результата
if code == 200 then
    print(table.concat(response_body))
else
    print("Ошибка: " .. (res or code))
end
```

## Глубокое Погружение
HTTP Basic Authentication — это метод, который позволяет HTTP-клиенту предоставлять имя пользователя и пароль при выполнении запроса. Появившийся в начале истории веба, он широко поддерживается, но не очень безопасен; учетные данные кодируются только в base64, а не шифруются.

Альтернативы включают Дайджест-аутентификацию, OAuth и API-ключи — все они обеспечивают более надежную безопасность. Базовая аутентификация обычно используется для быстрого тестирования скриптов, внутренних инструментов или в случаях, когда транспорт защищен через HTTPS.

Для реализации базовой аутентификации в Lua вы обычно создаете строку, объединяя имя пользователя и пароль с помощью двоеточия, затем кодируете эту строку в base64. Эта закодированная строка отправляется в заголовке `Authorization` вашего HTTP-запроса.

Гибкая природа Lua означает, что у вас есть выбор библиотек для обработки HTTP и кодирования в base64. LuaSocket долгое время был основным инструментом для сетевых операций, хотя в новых версиях Lua появляются альтернативы, такие как библиотека `http` или привязки `CURL` для более сложных задач.

## Смотрите также
- Документация LuaSocket: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaSec для поддержки HTTPS: https://github.com/brunoos/luasec/wiki
- Введение в HTTP-аутентификацию: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- RFC 2617 – HTTP-аутентификация: базовая и дайджест-аутентификация: https://tools.ietf.org/html/rfc2617
