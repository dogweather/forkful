---
date: 2024-01-20 17:44:27.934279-07:00
description: "\u4E0B\u8F7D\u7F51\u9875\u5C31\u662F\u4ECE\u4E92\u8054\u7F51\u4E0A\u83B7\
  \u53D6\u7F51\u9875\u7684\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u901A\
  \u5E38\u662F\u4E3A\u4E86\u81EA\u52A8\u5316\u5904\u7406\u4FE1\u606F\uFF0C\u4F8B\u5982\
  \u6570\u636E\u6316\u6398\u6216\u8005\u7F51\u9875\u5907\u4EFD\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.910384-06:00'
model: gpt-4-1106-preview
summary: "\u4E0B\u8F7D\u7F51\u9875\u5C31\u662F\u4ECE\u4E92\u8054\u7F51\u4E0A\u83B7\
  \u53D6\u7F51\u9875\u7684\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u901A\
  \u5E38\u662F\u4E3A\u4E86\u81EA\u52A8\u5316\u5904\u7406\u4FE1\u606F\uFF0C\u4F8B\u5982\
  \u6570\u636E\u6316\u6398\u6216\u8005\u7F51\u9875\u5907\u4EFD\u3002"
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

## What & Why? (什么以及为什么？)
下载网页就是从互联网上获取网页的内容。程序员这样做通常是为了自动化处理信息，例如数据挖掘或者网页备份。

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
