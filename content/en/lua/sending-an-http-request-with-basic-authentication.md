---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication in Lua is like knocking on a secure website's door using a username-password combo. This helps keep your access to data legit and safe.

## How to:

In Lua, you can use internet modules like ltn12, http, and socket.http. Here's an example using LuaSocket's HTTP client.

```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")

local url = 'http://example.com'
local user_pass = 'user:password' -- replace with your actual username and password
local authorization = 'Basic ' .. (user_pass):gsub("(%w+)", {['+']=' '})

http.request {
    url = url,
    headers = { authorization = authorization },
    sink = ltn12.sink.file(io.stdout)
}
```
This code sends a GET request to 'http://example.com' with basic authentication. The server's response is printed to the console.

## Deep Dive

Basic authentication is as old as the web itself. Essentially, it slaps Base64-encoded 'username:password' into an Authorization header. Super simple but not mega secure, so always use HTTPS with it.

Several Lua modules can send HTTP requests, like LuaSec and LuaSocket. LuaSec is a beefed-up LuaSocket with SSL/TLS. LuaSocket alone should do for HTTP, but LuaSec secures you for HTTPS.

Lua doesn't have base64 encoding built in, but we can cheat a little using the gsub method like in the example code. It replaces '+' with a space in the 'username:password' string before it gets into the final authorization header.

## See Also

* [Github: LuaSocket](https://github.com/diegonehab/luasocket)
* [Github: LuaSec](https://github.com/brunoos/luasec)
* [RFC 7617: The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
* [What is Basic Authorization](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)