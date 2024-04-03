---
date: 2024-01-20 18:02:22.448414-07:00
description: "How to: (\u600E\u4E48\u505A\uFF1A) Lua\u4E2D\u53D1\u9001\u5E26\u57FA\
  \u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\u9700\u8981\u7528\u5230\u989D\u5916\u7684\
  \u5E93\uFF0C\u5982`socket.http`\u548C`mime`\u3002\u8FD9\u91CC\u4F7F\u7528`lua-requests`\u7B80\
  \u5316\u8FC7\u7A0B\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.911413-06:00'
model: gpt-4-1106-preview
summary: "Lua\u4E2D\u53D1\u9001\u5E26\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\
  \u9700\u8981\u7528\u5230\u989D\u5916\u7684\u5E93\uFF0C\u5982`socket.http`\u548C\
  `mime`\u3002\u8FD9\u91CC\u4F7F\u7528`lua-requests`\u7B80\u5316\u8FC7\u7A0B\uFF1A\
  ."
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

## How to: (怎么做：)
Lua中发送带基本认证的HTTP请求需要用到额外的库，如`socket.http`和`mime`。这里使用`lua-requests`简化过程：

```Lua
local requests = require('requests')

function get_with_basic_auth(url, username, password)
    local response = requests.get(url, {auth = {username, password}})
    return response
end

-- 使用你的URL和认证信息替换下面的代码
local url = 'http://example.com/api/data'
local username = 'user'
local password = 'pass'
local result = get_with_basic_auth(url, username, password)

print(result.status_code)
print(result.text)
```
执行后，你会看到状态码和获得的数据。

## Deep Dive (深入探索)
早期HTTP没有内建的认证机制，随后引入了基本认证来提供简单的用户名和密码验证。虽然不是最安全，但基本认证由于其简单性仍广泛使用。替代方案有OAuth等更复杂的认证机制。Lua中处理HTTP请求通常需要依赖外部库，`lua-requests`是模仿Python `requests`库的Lua库，用起来很方便。

## See Also (另请参阅)
- Lua `requests`库: [https://github.com/JakobGreen/lua-requests](https://github.com/JakobGreen/lua-requests)
- LuaSocket 参考手册: [http://w3.impa.br/~diego/software/luasocket/http.html](http://w3.impa.br/~diego/software/luasocket/http.html)
- HTTP基本认证介绍: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
