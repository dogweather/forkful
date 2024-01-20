---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
发送HTTP请求就是让编程语言通过Internet传递信息的方式. 我们之所以这么做，是因为它允许我们获取或发送网络服务器上的信息。

## 如何做：
通过Fish Shell，我们可以使用环境命令 `curl` 来发送HTTP请求. 以下是一个简单的示例:

```Fish Shell
curl http://example.com
```

你会得到这样的结果:

```Fish Shell
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
   ...
</html>
```

## 深度剖析：
发送HTTP请求有着积极的历史背景，它最初是为了使网络通信变得更加简单和便捷. 除了 `curl`，我们还有其他一些选择，例如 `wget` 或者 `httpie`。 在 Fish Shell 中发送HTTP请求的实现细节其实就是通过网络协议向服务器发送一些预先设定的数据。

## 参考链接：
2. [HTTPie – command line HTTP client](https://httpie.org/)
3. [Wget command Examples](https://www.tecmint.com/10-wget-command-examples-in-linux/)