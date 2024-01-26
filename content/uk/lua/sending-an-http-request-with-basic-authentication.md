---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
date:                  2024-01-20T18:02:10.161830-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Sending an HTTP request with basic authentication involves attaching a username and password to the request header. Programmers use it to access resources that require user verification.

## How to: (Як це зробити:)
```Lua
-- Requires the 'socket.http' and 'ltn12' libraries for HTTP and LTN12 for sinks.
local http = require("socket.http")
local ltn12 = require("ltn12")

-- Encode your credentials in base64.
local function encodeBase64(source_str)
    local b64chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
    local s = string.gsub(source_str, "(.)", function(x) 
        local r, b = "", x:byte()
        for i = 8, 1, -1 do r = r .. (b % 2^i - b % 2^(i-1) > 0 and '1' or '0') end
        return r; 
    end)

    s = string.gsub(s, '.......', function(x) if (#x < 7) then return '' end; return b64chars:sub(tonumber(x, 2) + 1, tonumber(x, 2) + 1) end)
  
    local pad = 2 - (string.len(s) % 4)
    if pad < 2 then s = s .. string.rep('=', pad) end
  
    return s
end

local username = "your_username"
local password = "your_password"
local encoded_credentials = encodeBase64(username .. ":" .. password)

-- Prepare headers with Basic Authentication.
local headers = {
    ["Authorization"] = "Basic " .. encoded_credentials
}

-- Your desired URL endpoint.
local url = "http://yourapiendpoint.com/data"

-- The table to store the response's body.
local response_body = {}

-- Send the request.
local res, code, response_headers = http.request{
    url = url,
    method = "GET",
    headers = headers,
    sink = ltn12.sink.table(response_body)
}

-- Output the result.
if code == 200 then
    print(table.concat(response_body))
else
    print("Failed with error code: ".. (code or "nil"))
end
```

## Deep Dive (Поглиблений Розбір):
HTTP Basic Authentication is a simple technique from the early days of the web. It's straightforward but not secure over HTTP; use HTTPS to prevent credentials from being transmitted in clear text. Handling authentication in Lua without an external library means manually encoding credentials and crafting HTTP headers. Libraries like `LuaSec` and higher-level HTTP clients such as `Lua-Requests` provide alternatives for more complex use cases, better security practices, and convenience.

The example above circumvents Lua's lack of native base64 encoding. Libraries like `luacrypto` can streamline this with pre-built functions. Remember, when sending sensitive information, using well-established libraries can mitigate security risks.

Finally, the response is parsed using `ltn12` sinks to handle the data as it streams in, providing a flexible way to manage the response body.

## See Also (Додаткові Ресурси):
- LuaSocket documentation: [http://w3.impa.br/~diego/software/luasocket/http.html](http://w3.impa.br/~diego/software/luasocket/http.html)
- LuaSec for SSL support: [https://github.com/brunoos/luasec/wiki](https://github.com/brunoos/luasec/wiki)
- Lua-Requests for a more Python-requests like library: [https://github.com/JakobGreen/lua-requests](https://github.com/JakobGreen/lua-requests)
- Base64 encoding explanation and more robust methods: [https://en.wikipedia.org/wiki/Base64](https://en.wikipedia.org/wiki/Base64)
