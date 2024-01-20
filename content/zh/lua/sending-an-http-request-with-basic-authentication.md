---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么和为什么？
基本验证的HTTP请求是一种在发送请求时附带用户凭证（用户名和密码）的方式。程序员之所以这么做，是因为这样可以访问受到身份验证保护的网络资源。

## 如何操作？
在 Lua 中，我们可以使用 `lua-http` 库来实现这个功能。先安装 `lua-http` 库：
```Lua
luarocks install http
```
下面是一个发送带有基本身份验证的 GET HTTP 请求的例子：
```Lua
local http_request = require "http.request"

local req = http_request.new_from_uri("http://example.com/")
req.headers:upsert(":method", "GET")
req.headers:upsert("authorization", "Basic " .. ("user:password"):base64())

local headers, stream = req:go()
print(headers:get ":status")  -- 可以打印状态码
```
请确保把上述代码中的 `"user:password"` 替换为你自己的用户名和密码。

## 深入探究
历史背景：HTTP基本身份验证的设计是基于早期的互联网，当时的重点是简单性而不是安全性。

替代方法：在无需确保最高级别安全性的情况下，可使用基本验证。对于敏感数据，应使用更安全的验证方式，如OAuth或JWT。

实施细节：`lua-http` 库使用 Lua 的协程功能。它在需要等待IO操作时挂起当前任务，这样可以在保持代码简洁性的同时，提高了执行效率。

## 另请参阅
相关资源：
- Lua `http_request` 官方文档： http://daurnimator.github.io/lua-http/
- HTTP 基本访问验证详解： https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication
- `lua-http` 库的GitHub页： https://github.com/daurnimator/lua-http

请务必参阅以上链接以获得更深入的理解和信息。