---
title:                "Wysyłanie żądania HTTP"
aliases:
- /pl/lua/sending-an-http-request/
date:                  2024-01-20T18:00:15.633151-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP to proces komunikacji z serwerem w sieci. Programiści robią to, by pobierać dane, wysyłać formularze lub interaktywnie komunikować się z aplikacjami webowymi.

## Jak to zrobić:
Do wysyłania żądań HTTP w Lua użyjemy biblioteki `socket.http`. Przykładowy kod:

```Lua
local http = require("socket.http")

-- Proste żądanie GET
local response, status, headers = http.request("http://httpbin.org/get")
if status == 200 then
    print("Odpowiedź:", response)
else
    print("Błąd:", status)
end

-- Żądanie POST
local body = "key1=value1&key2=value2"
headers = {
    ["Content-Type"] = "application/x-www-form-urlencoded",
    ["Content-Length"] = #body
}
response, status = http.request("http://httpbin.org/post", body, headers)
if status == 200 then
    print("Odpowiedź POST:", response)
else
    print("Błąd POST:", status)
end
```

Przykładowe wyjście:
```
Odpowiedź: {...json z odpowiedzią...}
Odpowiedź POST: {...json z odpowiedzią...}
```

## Deep Dive
W Lua, wysyłanie żądań HTTP często wspiera biblioteka `LuaSocket`. Historia sięga 2004 roku i jest standardem w operacjach sieciowych w Lua. Inne opcje to `lua-http` czy `luasec` dla HTTPS. Ważne, by sprawdzić kompatybilność z wersją Lua.

Co do implementacji, `socket.http` jest blokującą biblioteką. Czyli, czeka na odpowiedź serwera, zawieszając inne procesy. W przypadkach gdzie to problematyczne, istnieją asynchroniczne alternatywy, takie jak `copas` czy `lunasync`.

## Zobacz też
- LuaSocket: http://w3.impa.br/~diego/software/luasocket/
- Dokumentacja LuaSocket: http://w3.impa.br/~diego/software/luasocket/http.html
- lua-http: https://github.com/daurnimator/lua-http
- LuaSec (dla HTTPS): https://github.com/brunoos/luasec
