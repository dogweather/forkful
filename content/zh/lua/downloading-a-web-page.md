---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

****
## 什么&为什么？
下载网页是获取网页内容的过程，保存为本地文件。程序员这么做是为了离线访问，数据采集，或进行网页分析。

## 如何操作：
Lua可通过http请求获取网页内容，例如使用socket.http库：

```Lua
--首先，加载socket.http库
http = require('socket.http')

--URL指定你想要下载的网页
URL = "http://www.example.com" 

--通过http.request函数下载网页内容
content = http.request(URL)

--输出网页内容
print(content)
```

在运行上述代码后，你应该能看到example.com网页的HTML代码在终端中打印出来。

##深入了解
下载网页在各种编程语言中都是常见的任务，但Lua的灵活性和轻巧，使它在网络编程中特别好用。但Lua并非唯一可以用来下载网页的语言，其他如Python的requests库，JavaScript的axios也都是不错的选择。

获取网页内容只是开始，你还可以使用Lua或其他语言对获取到的内容做进一步的处理，例如分析HTML，提取信息等。

##另请参见
- Lua官方文档：http://www.lua.org/docs.html
- Lua socket.http库详细介绍：http://w3.impa.br/~diego/software/luasocket/http.html
- 如何使用Lua进行网络编程的更多信息：https://www.tutorialspoint.com/lua/lua_networking.htm
****