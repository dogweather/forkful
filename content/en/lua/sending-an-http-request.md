---
title:                "Sending an http request"
html_title:           "Lua recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
When programming, sending an HTTP request simply means asking a web server for specific information or functionality. Programmers do this to retrieve data, interact with APIs, or perform web-based tasks within their code.

## How to:
```Lua
-- To make an HTTP request in Lua, we first need to load the "socket" library.
local socket = require("socket")

-- Next, we create a TCP connection with the server we want to send the request to.
-- In this example, we will use Google's homepage as our server.
local connection = socket.tcp()
connection:connect("www.google.com", 80)

-- Now, we'll construct our HTTP request using the appropriate format.
-- The first line contains the request type (GET, POST, etc.) and the path to the resource we want.
local request = "GET / HTTP/1.1\r\n"

-- We can also include specific headers in our request, such as the "Host" and "User-Agent".
request = request .. "Host: www.google.com\r\n" .. "User-Agent: Lua HTTP Request\r\n"

-- Lastly, we add an empty line to signify the end of our request.
request = request .. "\r\n"

-- We can now send our request through the TCP connection.
connection:send(request)

-- Lastly, we'll read and print the response from the server.
local response = connection:receive("*a")
print(response)

-- Remember to close the connection once we're done.
connection:close()
```

The above code creates a basic HTTP request and receives the response from the server. However, this is a simplified example and does not include error handling or more complex HTTP methods.

## Deep Dive:
Sending HTTP requests has been a fundamental part of web development since the early days of the internet. It allows for communication between different systems over the Hypertext Transfer Protocol (HTTP).

Although Lua does not have a built-in HTTP library, there are multiple popular community-maintained libraries available, such as "luasocket" and "Lua-cURL". These libraries provide more advanced features for handling HTTP requests, such as authentication, cookies, and timeouts.

Alternatively, some Lua frameworks, like "OpenResty", have built-in HTTP client functionality. These libraries and frameworks are often preferred over manually creating HTTP requests, as they handle more complex scenarios and provide a more streamlined approach.

## See Also:
- LuaSocket library: http://w3.impa.br/~diego/software/luasocket/
- Lua-cURL library: https://github.com/Lua-cURL/Lua-cURLv
- OpenResty framework: https://openresty.org/