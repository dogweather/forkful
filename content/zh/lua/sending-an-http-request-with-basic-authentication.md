---
title:                "使用基本认证发送http请求"
html_title:           "Lua: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么和为什么？
发送带有基本身份验证的HTTP请求是指程序员使用特定的用户名和密码在发送网络请求时，提供身份确认信息。程序员这样做的原因是为了保护网络通信的安全性和私密性。

## 如何：
下面是使用Lua编写发送带有基本身份验证的HTTP请求的示例代码和输出结果：

```Lua
-- 导入http库
local http = require("socket.http")

-- 设置请求URL和需要传递的认证信息
local url = "https://example.com"
local auth = "username:password"

-- 发送GET请求
local response, code, headers = http.request{
    url = url,
    headers = {
        -- 使用Basic认证方式
        ["Authorization"] = "Basic " .. (mime.b64(auth))
    }
}

-- 输出结果
print("Response: " .. response) -- 返回服务器的响应
print("Code: " .. code) -- 返回状态码
for k,v in pairs(headers) do -- 输出返回的Header信息
    print(k,v)
end
```

输出结果可能如下所示：

```Lua
Response: hello world!
Code: 200
Content-Type    text/plain
Content-Length  12
Date            Tue, 22 Sep 2020 12:00:00 GMT
```

## 深入探讨：
发送带有基本身份验证的HTTP请求是一种早期的HTTP协议安全措施，它可以通过对用户名和密码进行加密后传输来防止信息被恶意截获。另外，还有一种更常用的方式，即使用HTTPS协议来保护网络通信的安全性。但是，基本身份验证仍然经常被用于简单的认证需求，例如访问API接口。

## 链接：
- [Lua官方文档](https://www.lua.org/)
- [http库文档](http://w3.impa.br/~diego/software/luasocket/http.html)
- [Basic认证介绍](https://en.wikipedia.org/wiki/Basic_access_authentication)