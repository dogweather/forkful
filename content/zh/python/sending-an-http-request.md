---
title:                "Python: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

为什么：发送HTTP请求是在Python编程中很常见的任务，因为它可以让我们连接到互联网上的其他系统，并获取或发送数据。所以它是构建强大的网络应用程序的基础。
 
## 如何：首先，我们需要导入Python的requests库。然后，我们可以使用`requests.get()`函数来发送GET请求并获取数据。下面是一个示例代码：
 
```Python
import requests

#发送GET请求
response = requests.get("https://www.google.com")

#将响应的状态码打印出来
print(response.status_code)

#将响应的内容打印出来
print(response.text)
```
 
输出：
 
```shell
200
<html>...
```

## 深入了解：发送HTTP请求也有很多其他的方法，比如POST请求、设置请求头信息等等。我们可以使用requests库来完成这些任务。例如，发送一个POST请求并附带一些参数：
 
```Python
import requests

#发送POST请求
response = requests.post("https://www.example.com/login", data={"username": "John", "password": "123456"})

#将响应的内容打印出来
print(response.text)
```
 
输出：
 
```shell
Welcome back, John!
```

另外，我们还可以使用requests库来设置请求头信息，例如添加User-Agent来模拟不同的浏览器。下面是一个示例代码：
 
```Python
import requests

#设置请求头信息
headers = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"}

#发送带有请求头信息的GET请求
response = requests.get("https://www.example.com", headers=headers)

#将响应的内容打印出来
print(response.text)
```

输出：

```shell
<html>...
```

## 查看也可以：如果您想深入了解更多关于发送HTTP请求的信息，可以查看以下链接：
- [Requests Documentation](https://requests.readthedocs.io/en/master/)
- [MDN Web Docs: HTTP requests](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [Real Python: Making HTTP Requests in Python](https://realpython.com/python-requests/)
 
如果您想学习更多有关Python编程的知识，请查看以下链接：
- [Python官方文档](https://docs.python.org/zh-cn/3/)
- [菜鸟教程：Python教程](https://www.runoob.com/python/python-tutorial.html)
- [实验楼: Python教程](https://www.lanqiao.cn/courses/1051)