---
title:                "Downloading a web page"
html_title:           "Lua recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page means retrieving its code and content from the internet, which can then be used for various purposes such as scraping data or automating tasks. Programmers do this to extract information from websites, automate tasks, or collect data to use in their applications.

## How to:

To download a web page in Lua, we will be using the built-in HTTP library. To get started, we need to require the library and specify the URL of the web page we want to download.

```Lua
local http = require("socket.http")
local url = "https://www.example.com"

```

Next, we can use the `request` function to send an HTTP request to the specified URL and return the response as a string.

```Lua
local response = http.request(url)

```

Finally, we can print the response to see the code and content of the web page.

```Lua
print(response)
```

Sample output:

```
<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
...
<body>
<h1>Example Domain</h1>
<p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
...
</body>
</html>
```

## Deep Dive:

Historically, downloading web pages was done using the `wget` utility or the `curl` library. However, with Lua's built-in `http` library, developers don't need to install any additional dependencies.

There are also alternative libraries available, such as `LuaSocket` and `LuaCURL`, which provide more advanced features for web requests.

When using the `request` function, the first argument is the URL, but we can also pass in additional arguments to customize the request, such as headers, cookies, and request methods. Additionally, the response object can be further parsed and processed to extract specific information.

## See Also:

- [Lua HTTP Library Documentation](https://www.lua.org/pil/22.3.html)
- [LuaSocket Library](https://luarocks.org/modules/luasocket/luasocket)
- [LuaCURL Library](https://luarocks.org/modules/curl/curl)