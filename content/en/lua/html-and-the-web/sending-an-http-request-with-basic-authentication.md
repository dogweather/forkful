---
date: 2024-01-20 18:02:10.489879-07:00
description: "Sending an HTTP request with basic authentication is when you make a\
  \ call to a web server, including a username and password for access. Programmers\
  \ do it\u2026"
lastmod: 2024-02-19 22:05:18.666934
model: gpt-4-1106-preview
summary: "Sending an HTTP request with basic authentication is when you make a call\
  \ to a web server, including a username and password for access. Programmers do\
  \ it\u2026"
title: Sending an HTTP request with basic authentication
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication is when you make a call to a web server, including a username and password for access. Programmers do it to interact with web services that require user verification before providing data or services.

## How to:

Lua doesn't have built-in HTTP support, so you'll need an external library like `socket.http` from LuaSocket or `http.request` from the `http` library if you're using Lua 5.3+. For basic auth, encode credentials and add them in the request header.

```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")
local mime = require("mime")

-- Your credentials
local username = "Aladdin"
local password = "openSesame"
local credentials = mime.b64(username .. ":" .. password)

-- Request setup
local response_body = {}
local res, code, response_headers = http.request{
    url = "http://example.com/data",
    method = "GET",
    headers = {
        ["Authorization"] = "Basic " .. credentials
    },
    sink = ltn12.sink.table(response_body)
}

-- Output result
if code == 200 then
    print(table.concat(response_body))
else
    print("Error: " .. (res or code))
end
```

## Deep Dive

HTTP Basic Authentication is a method for an HTTP user agent to provide a user name and password when making a request. Invented early in the web's history, it's widely supported but not very secure; credentials are only base64-encoded, not encrypted.

Alternatives include Digest Authentication, OAuth, and API keys – all of which provide stronger security. Basic auth is commonly used for scripting quick tests, internal tools, or where the transport is secured via HTTPS.

To implement basic authentication in Lua, you typically build a string combining the username and password separated by a colon, then encode that string with base64. This encoded string is sent in the `Authorization` header of your HTTP request.

Lua's flexible nature means you have choices on libraries to handle HTTP and base64 encoding. LuaSocket has been the go-to for network operations for a long time, though newer versions of Lua introduce alternatives like the `http` library or `CURL` bindings for more complex tasks.

## See Also

- LuaSocket Documentation: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaSec for HTTPS support: https://github.com/brunoos/luasec/wiki
- An intro to HTTP Authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- RFC 2617 – HTTP Authentication: Basic and Digest Access Authentication: https://tools.ietf.org/html/rfc2617
