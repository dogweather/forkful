---
date: 2024-01-20 17:44:27.934279-07:00
description: "How to: (\u5982\u4F55\u53BB\u505A\uFF1A) \u5728Lua\u4E2D\u4E0B\u8F7D\
  \u7F51\u9875\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528`socket.http`\u6A21\u5757\u3002\u4E0B\
  \u9762\u662F\u4E00\u4E2A\u57FA\u7840\u793A\u4F8B\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.215173-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u53BB\u505A\uFF1A) \u5728Lua\u4E2D\u4E0B\u8F7D\u7F51\u9875\
  \uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528`socket.http`\u6A21\u5757\u3002\u4E0B\u9762\u662F\
  \u4E00\u4E2A\u57FA\u7840\u793A\u4F8B\uFF1A."
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

## How to: (如何去做：)
在Lua中下载网页，你可以使用`socket.http`模块。下面是一个基础示例：

```Lua
local http = require("socket.http")
local body, code = http.request("http://www.example.com")
if code == 200 then
    print(body)
else
    print("Error downloading: "..tostring(code))
end
```
样本输出：
```
<!doctype html>
<html>
...
</html>
```

## Deep Dive (深入探究)
在Lua早期版本中，没有内建的HTTP支持，所以需要使用外部库。现在，LuaRocks这样的包管理器使安装`socket.http`变得简单。还有其他方法下载数据，如使用`io.popen`调用`curl`或`wget`，但直接用Lua是更干净的解决方案。实现时，记得处理HTTP状态码，例如404表示页面未找到，503表示服务不可用。

## See Also (另请参阅)
- LuaRocks官网: [https://luarocks.org/](https://luarocks.org/)
- LuaSocket文档: [http://w3.impa.br/~diego/software/luasocket/http.html](http://w3.impa.br/~diego/software/luasocket/http.html)
- 更多Lua资源: [https://www.lua.org/start.html](https://www.lua.org/start.html)
