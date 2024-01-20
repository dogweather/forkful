---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
发送带有基本身份验证的 HTTP 请求是一种用于访问防护网络上的信息的编程方法。程序员在需要访问被受密码保护的资源，例如某个 API，文件，或是网站时，用这种方法进行处理。

## 如何实现？
在 PowerShell 中，我们可以用 `Invoke-WebRequest`、用户名和密码进行 HTTP 基本身份认证的请求。下面是一个基本示例：
```PowerShell
$URI = 'http://example.com'
$Username = 'username'
$Password = 'password'
$pair = "${Username}:${Password}"
$encodedCreds = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($pair))
$basicAuthValue = "Basic $encodedCreds"
$headers = @{Authorization = $basicAuthValue}
$response = Invoke-WebRequest -Uri $URI -Headers $headers
```

所得到的 `$response` 就是服务器的响应，其中包含了响应的主体，状态码等信息。

## 深入研究
HTTP 基本身份验证由万维网联盟 (W3C) 实施的早期互联网标准，以简单但有效的方式验证用户身份。虽然它更为简单和直接，但由于安全性问题，今天这种方法往往被比如 OAuth 和 JWT 等更加安全的方法所替代。

在 PowerShell 中，除了上面提到的 `Invoke-WebRequest`，还可以使用 `Invoke-RestMethod` 来发送网络请求。这两者的主要区别是 `Invoke-WebRequest` 返回详细的 HTTP 响应，而 `Invoke-RestMethod` 仅返回响应内容。

尽管 PowerShell 在发送 HTTP 请求进行身份验证时表现良好，但任何涉及密码传输的处理都应考虑安全性。HTTP 基本身份验证会将用户名密码以明文形式通过网络发送，哪怕它们是经过 Base64 编码的。由于基础身份验证没有任何加密措施，建议仅在 HTTPS 等加密连接中使用。

## 另请参阅
1. PowerShell 官网上详细介绍 `Invoke-WebRequest` 的文档: https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7
2. 关于基本身份验证的更多信息: https://tools.ietf.org/html/rfc2617
3. `Invoke-RestMethod` 详细文档: https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/invoke-restmethod
4. 关于在 PowerShell 中处理 HTTPS 的详细指南: https://ss64.com/ps/syntax-https.html