---
date: 2024-01-20 18:00:26.962788-07:00
description: Sending an HTTP request is a way to communicate with web servers. Programmers
  do this to fetch data, submit forms, or interact with APIs.
lastmod: '2024-03-13T22:44:49.494219-06:00'
model: gpt-4-1106-preview
summary: Sending an HTTP request is a way to communicate with web servers.
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

## How to: / Як це зробити:
```Lua
-- Ensure you have the 'lua-socket' library
local http = require("socket.http")
local ltn12 = require("ltn12")

-- Simple GET request
local response_body = {}
local res, code, response_headers = http.request{
    url = "http://httpbin.org/get",
    sink = ltn12.sink.table(response_body)
}

-- Check if the request succeeded
if code == 200 then
    print("Success:")
    print(table.concat(response_body))
else
    print("Failed:", code)
end

-- POST request with data
local response_body_post = {}
res, code = http.request{
    url = "http://httpbin.org/post",
    method = "POST",
    headers = {
        ["Content-Type"] = "application/x-www-form-urlencoded"
    },
    source = ltn12.source.string("key1=value1&key2=value2"),
    sink = ltn12.sink.table(response_body_post)
}

-- Check if the POST request succeeded
if code == 200 then
    print("POST Success:")
    print(table.concat(response_body_post))
else
    print("POST Failed:", code)
end
```

Sample output for GET request:

```
Success:
{
  "args": {}, 
  "headers": {
    "Host": "httpbin.org",
    ...
  },
  ...
}
```

Sample output for POST request:

```
POST Success:
{
  "form": {
    "key1": "value1",
    "key2": "value2"
  },
  ...
}
```

## Deep Dive / Глибоке Занурення:
HTTP requests are the backbone of web communication, dating back to the 1990s. Choices for sending HTTP requests in Lua include libraries like `lua-socket` for simple interactions, and `lua-http` for more complex tasks. The `lua-socket` library's `http.request` function handles the basic request-and-response cycle. It uses sockets to send and receive data, which can be manipulated with Lua streams (implemented by the `ltn12` module). Both the request and the response can have headers to pass additional info (like content type). Error handling is essential; always check the status code (`code`) to make sure your request was successful.

## See Also / Дивіться також:
- LuaSocket HTTP Documentation: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaSocket GitHub Repository: https://github.com/diegonehab/luasocket
- HTTPBin for HTTP Request & Response Service Testing: https://httpbin.org/
- Lua 5.4 Reference Manual: https://www.lua.org/manual/5.4/
- LuaSec for HTTPS requests: https://github.com/brunoos/luasec
