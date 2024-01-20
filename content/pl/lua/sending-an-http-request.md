---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP to proces, w którym komputer (klient) żąda danych od serwera poprzez protokół HTTP. Programiści robią to, aby komunikować się z serwerami internetowymi, pobierać dane lub wysyłać informacje do serwera.

## Jak to zrobić:

Możesz wysłać żądanie HTTP w Lua za pomocą biblioteki `socket.http`. Poniżej przedstawiam przykład prostej żądania GET:

```Lua
local http = require("socket.http")

-- Adres URL, z którego chcemy pobrać dane
local url = "http://example.com"

-- Wyślij żądanie HTTP GET
local body, statusCode, headers, statusText = http.request(url)

-- Wydrukuj odpowiedź
print(statusCode, statusText)
print(body)
```
Gdy uruchomisz powyższy kod, zobaczysz coś takiego:
```Lua
200    OK
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
   ...
```

## W głąb tematu

Wysyłanie żądań HTTP jest ważnym elementem komunikacji sieciowej od wprowadzenia protokołu HTTP w 1991 roku. O ile Lua nie ma wbudowanego wsparcia dla HTTP, biblioteki takie jak `socket.http` umożliwiają łatwe tworzenie i wysyłanie żądań.

Alternatywą dla `socket.http` może być biblioteka `luajit-request`, która oferuje większą elastyczność, ale może wymagać więcej konfiguracji. Istnieją też biblioteki, które umożliwiają współpracę z innymi protokołami, takimi jak HTTPS.

Szczegóły implementacji bibliotek HTTP w Lua zależą od biblioteki. Na przykład, `socket.http` korzysta z TCP sockets do nawiązania połączenia sieciowego, a następnie wysyła surowe dane HTTP do serwera.

## Zobacz także 

- [Lua Users Wiki: HTTP luasocket example](http://lua-users.org/wiki/HttpLuaSocketExample) 
- [Lua Documentation: Programming in Lua (Networking)](https://www.lua.org/pil/27.1.html) 
- [luajit-request on GitHub](https://github.com/LPGhatguy/luajit-request)