---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page means fetching it from the server it's hosted on to your local machine. We do this to work with a site's data offline, automate tasks, mine data--you name it.

## How to:

Lua mostly lacks built-in tools for downloading web pages, so we use `LuaSocket` and `LuaSec`, libraries geared towards network programming. Install them with LuaRocks package manager:

```
>luarocks install luasocket
>luarocks install luasec
```

Here's some easy-to-follow Lua code which employs the http module from these libraries:

```Lua
local https = require('ssl.https') --https module
local body, code = https.request("https://wikipedia.org")

if code == 200 then
    print(body)
else
    print("HTTP request failed with code: " .. code)
end
```

This script sends a simple GET request to `https://wikipedia.org` and prints the page's HTML if successful. If it fails, you'll see an error code instead.

## Deep Dive

Lua doesn't come with an HTTP API historically since it's designed to be small and extensible, prioritizing simplicity over a wide range of features.

For downloading web pages, alternatives exist like the `wget` utility, `curl` library, but these require external dependencies or aren't as simple to use.

Implementation details to know:

- The `https.request()` function attempts to connect to the provided URL, sends an HTTP GET request, and gets the response.
- The function returns the whole content in a string (body) and a status code (code).
- It's essential to check the status code. `200` signifies success, any other indicates an error.

## See Also

- LuaSocket documentation: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaSec documentation: https://github.com/brunoos/luasec/wiki
- HTTP status codes: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
- More about Lua: https://www.lua.org/about.html

This article gives you a starting point for downloading web pages in Lua. Explore these resources and dive deeper! There's always more to learn.