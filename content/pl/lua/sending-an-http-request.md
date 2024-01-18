---
title:                "Wysyłanie żądania http"
html_title:           "Lua: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP jest podstawowym elementem tworzenia aplikacji internetowych za pomocą języka programowania Lua. Programiści używają tego procesu, aby komunikować się z innymi serwerami lub zasobami w sieci, takimi jak bazy danych lub zewnętrzne usługi.

## Jak to zrobić:
### Przykład 1:
Wysłanie prostej prośby GET do wewnętrznego adresu URL z wykorzystaniem biblioteki HTTP w Lua:
```Lua
local http = require("resty.http")
local client = http.new()
local res, err = client:request_uri("http://localhost:8080/api")
ngx.say(res.body) 
```
Wynik:
`Hello World!`

### Przykład 2:
Wysłanie żądania POST z danymi do zewnętrznego API przy użyciu biblioteki HTTP w Lua:
```Lua
local http = require("resty.http")
local client = http.new()
local res, err = client:request_uri("https://example.com/api", {
    method = "POST",
    body = "name=John&age=30",
    headers = {
        ["Content-Type"] = "application/x-www-form-urlencoded",
    },
})
ngx.say(res.body) 
```
Wynik:
`{"message": "Success"}`

## Głębokie zanurzenie:
Wysyłanie żądań HTTP stało się powszechne ze wzrostem popularności aplikacji internetowych. Wcześniej w języku Lua nie było wbudowanych funkcji do obsługi sieci, dlatego programiści korzystali z zewnętrznych bibliotek, takich jak LuaSocket czy copas. Obecnie istnieje wiele bibliotek do obsługi żądań HTTP, a jedna z popularniejszych jest resty.http, oparta na asynchronicznej architekturze. Do wysłania żądania można również użyć wbudowanego w język Lua modułu os, ale wymagałoby to ręcznego tworzenia protokołu HTTP.

## Zobacz także:
- https://github.com/pintsized/lua-resty-http - biblioteka resty.http dla obsługi żądań HTTP w języku Lua
- https://www.lua.org/ - oficjalna strona języka Lua
- https://cloudflare.github.io/lua-resty-http/api.html - dokumentacja dla biblioteki resty.http