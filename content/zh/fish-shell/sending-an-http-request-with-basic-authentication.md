---
title:                "使用基本身份验证发送http请求"
html_title:           "Fish Shell: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 为什么

发送带基本身份验证的HTTP请求有什么用？你可能会问。实际上，这是一种安全的方法来访问需要身份验证的网站，如API端点或可访问的受保护资源。

# 如何操作

用Fish Shell发送带基本身份验证的HTTP请求非常简单。首先，你需要安装并启用HTTPie插件，这样就可以使用Fish Shell来执行HTTP请求。然后，按照以下格式设置请求：

```
fish -c 'http -a [用户名]:[密码] [URL]'
```

在这个命令中，你需要将[用户名]替换为实际的用户名，[密码]替换为密码，[URL]替换为带有身份验证的请求URL。你可以在命令行中直接输入这个命令，或者将它添加到Fish Shell配置文件中，这样就可以在每次使用时自动执行。

例如，假设你想要访问一个需要用户名为"username"，密码为"password"的API端点。你可以使用以下命令：

```
fish -c 'http -a username:password http://api.example.com'
```

这样，你就可以成功发送带基本身份验证的HTTP请求。

# 深入了解

要想更深入地了解如何使用Fish Shell发送带基本身份验证的HTTP请求，你可以查看HTTPie插件的文档。其中包含了更多的选项和示例，帮助你更有效地处理不同的HTTP请求。

同时，你也可以了解更多关于基本身份验证的细节，比如如何使用安全的密码，以及如何在请求中使用其他身份验证类型，如Bearer Token。

# 参考链接

- [Fish Shell官方网站](https://fishshell.com/)
- [HTTPie插件文档](https://github.com/jorgebucaran/fish-httpie)
- [HTTP基本身份验证](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication)