---
title:                "Sending an http request"
date:                  2024-01-20T18:00:16.334061-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an http request"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request means asking a remote server for data or action. Programmers do this to interact with web services, fetch resources, or communicate with APIs.

## How to:

Lua doesn't have built-in HTTP support, so we use libraries. One common choice is `lua-requests`. Here's a quick example:

```lua
local requests = require('requests')

-- GET request
local response = requests.get('https://api.example.com/data')
print(response.status_code)
print(response.text)

-- POST request with some data
local post_response = requests.post('https://api.example.com/post', {data = {key1 = 'value1', key2 = 'value2'}})
print(post_response.status_code)
print(post_response.text)
```

Sample output can look like this:

```lua
200
"{\"data\":\"Here's the data you requested!\"}"

201
"{\"success\":true,\"message\":\"Data received!\"}"
```

## Deep Dive

Lua's simplicity doesn't natively cover HTTP, which is where libraries step in. `lua-requests` mirrors the Python Requests library's functionality, making it a breeze for those familiar with Python.

Other alternatives include `LuaSocket` for lower-level HTTP work and `luasocket.http` for more control. Lua also has bindings for `libcurl` (via `Lua-cURL`) for complex HTTP operations.

Historically, lacking built-in HTTP support reflects Lua's embedded-system roots where network programming wasn't a priority. Its evolution through external libraries exemplifies the community's adaptability and the language's extensibility.

Implementation wise, when you send an HTTP request, it travels over the network to the specified server. The server processes it and replies. Lua libraries abstract the socket programming needed, handling all the nitty-gritty of network communication so you focus on the actual request and response.

## See Also

- [lua-requests GitHub repository](https://github.com/JakobGreen/lua-requests)
- [LuaSocket Reference Manual](http://w3.impa.br/~diego/software/luasocket/http.html)