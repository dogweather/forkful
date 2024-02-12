---
title:                "Downloading a web page"
date:                  2024-01-20T17:44:24.301004-07:00
model:                 gpt-4-1106-preview
simple_title:         "Downloading a web page"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Downloading a web page means grabbing the HTML content from the internet via its URL. Programmers do it to analyze web content, automate tasks, or integrate data into their apps.

## How to:
Lua isn't equipped out of the box for web tasks, but with the `socket` library and `http` module, it's a cinch. Here's a quick example using LuaSocket:

```Lua
-- Don't forget to install LuaSocket: `luarocks install luasocket`
local http = require("socket.http")
local body, code = http.request("http://www.example.com")

if code == 200 then
    print(body)  -- Success! Prints the web page content.
else
    print("Something went wrong :(", code)
end
```

Sample Output:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Deep Dive
Before LuaSocket, downloading web content in Lua was more cumbersome. Alternates like using `io.popen` to call `curl` or `wget` were common. 

LuaSocket has been around since 2004, making network interactions like HTTP requests straightforward in Lua. It works by wrapping TCP/IP socket API calls into easy-to-use Lua functions. For HTTPS, LuaSec can be layered on.

Lua's extensibility means you can also use other Lua-based frameworks or modules, like OpenResty for more complex web interactions within a high-performance web server environment.

Keep in mind, if you're doing hefty web scraping or complex processing, Lua may not be your go-to; Python with libraries like Requests and Beautiful Soup might serve you better.

## See Also
- LuaSocket documentation: http://w3.impa.br/~diego/software/luasocket/
- LuaSec (for HTTPS support): https://github.com/brunoos/luasec/wiki
- OpenResty for more advanced web interactions: https://openresty.org/en/
