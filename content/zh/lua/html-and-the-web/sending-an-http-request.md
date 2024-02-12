---
title:                "发出 HTTP 请求"
aliases: - /zh/lua/sending-an-http-request.md
date:                  2024-01-20T18:00:05.469025-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
发送HTTP请求让你从网上获取或发送信息。程序员这么做来交互应用数据，比如自动化任务或者使用网络API。

## How to: (如何操作)
Lua没有内建的HTTP功能。你需要一个外部库，比如LuaSocket。下面的例子使用LuaSocket发送一个GET请求。

```Lua
-- 引入LuaSocket库
local http = require("socket.http")

-- 发送一个GET请求到example.com
local response, status, headers = http.request("https://www.example.com")

-- 打印结果
if status == 200 then
    print("数据获取成功！")
    print(response)
else
    print("数据获取失败。状态码：" .. status)
end
```
输出可能是网站返回的HTML内容，或错误信息。

## Deep Dive (深入研究)
早期的Lua版本并没有网络功能。发送HTTP请求通常需要外部库，如LuaSocket，这是最著名的。Lua 5.1开始引入了模块机制，方便了外部库的使用。LuaSocket库支持TCP和UDP，能够完成大多数HTTP请求任务。还有其他库如Lua-cURL和HTTP模块，提供更多的HTTP客户端功能，比如处理cookies和连接池。

## See Also (另请参阅)
- LuaSocket官方文档： http://w3.impa.br/~diego/software/luasocket/
- Lua用户手册： https://www.lua.org/manual/5.4/
- HTTP模块： https://github.com/daurnimator/lua-http
