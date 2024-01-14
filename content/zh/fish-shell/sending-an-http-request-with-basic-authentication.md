---
title:                "Fish Shell: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 为什么

在进行网络编程时，我们经常需要向服务器发送 HTTP 请求。有时候，我们需要使用基本身份验证来验证我们的请求。这篇博文将向你介绍如何使用 Fish Shell 发送带有基本身份验证的 HTTP 请求。

# 如何

让我们假设我们需要向一个服务器发起 GET 请求，并且需要提供用户名和密码进行身份验证。首先，我们需要在 Fish Shell 中导入 curl 命令：

```Fish Shell
source curl@7.78.0/share/functions/init.ccurl
```

接下来，我们需要设置我们请求的 URL、用户名和密码：

```Fish Shell
set URL "https://www.example.com/api"
set USERNAME "username"
set PASSWORD "password"
```

现在，我们可以使用 curl 命令来发送带有基本身份验证的 HTTP 请求：

```Fish Shell
curl --basic --user $USERNAME:$PASSWORD $URL
```

这将会输出服务器返回的结果，如果身份验证成功，则会返回请求的内容。如果身份验证失败，则会返回错误信息。

# 深入了解

在 Fish Shell 中，我们可以通过 `--user` 参数来指定用户名和密码进行基本身份验证。`$USERNAME` 和 `$PASSWORD `是我们之前设置的变量，可以替换为实际的用户名和密码。如果你需要向服务器发送其他类型的身份验证请求，可以在 `--basic` 参数后面添加相应的参数。例如，如果你需要发送带有摘要身份验证的请求，可以使用 `--digest` 参数。

# 参考链接

- https://fishshell.com/docs/current/index.html