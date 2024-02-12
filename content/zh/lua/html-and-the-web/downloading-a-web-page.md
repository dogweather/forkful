---
title:                "下载网页"
aliases:
- /zh/lua/downloading-a-web-page.md
date:                  2024-01-20T17:44:27.934279-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

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
