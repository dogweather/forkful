---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request involves asking a server for a specific resource, often a web page or file, using the HyperText Transfer Protocol (HTTP). Programmers do this to interact with web services, retrieve data from a server, or send user data following actions like form submission.

## How to:

In Lua, we can use the `lua-http` library to send HTTP requests. Here's an example on how to send a simple GET request.

```Lua
local http_request = require "http.request"
local headers, stream = assert(http_request.new_from_uri("http://example.com"):go())
local body = assert(stream:get_body_as_string())
if headers:get ":status" ~= "200" then
    error(body)
end
print(body)
```
This code sends a GET request to http://example.com and prints the response body.

## Deep Dive

Sending HTTP requests is a cornerstone of web programming. Historically, HTTP started as a simple request-response protocol in the early days of the web. Now, in its current iteration (HTTP/2), it is far more complex and permits concurrent requests, significantly improving performance.

While `lua-http` is a great tool to directly control your HTTP requests and responses, other libraries such as `luasocket's http module` or `luajit-request` also provide similar functionality. Depending on your needs, especially in terms of compatibility and flexibility, your mileage may vary.

Lua doesn't have built-in HTTP support like Node.js or Python, so you'll need to include external libraries to work with HTTP. Knowing the right tool for your project and how it works under the hood provides better control and customization, ultimately helping you write better Lua code.

## See Also

- HTTP/2: https://en.wikipedia.org/wiki/HTTP/2
- lua-http: https://github.com/daurnimator/lua-http
- luasocket:http: http://w3.impa.br/~diego/software/luasocket/http.html
- luajit-request: https://github.com/LPGhatguy/luajit-request