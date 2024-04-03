---
date: 2024-01-20 18:02:57.945931-07:00
description: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\
  \u662F\u4E00\u79CD\u5BA2\u6237\u7AEF\u5411\u670D\u52A1\u7AEF\u4F20\u9012\u7528\u6237\
  \u540D\u548C\u5BC6\u7801\u4EE5\u83B7\u53D6\u8D44\u6E90\u7684\u65B9\u5F0F\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u8BBF\u95EE\u53D7\u5BC6\u7801\u4FDD\
  \u62A4\u7684\u7F51\u9875\u6216API\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.013120-06:00'
model: gpt-4-1106-preview
summary: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\u662F\
  \u4E00\u79CD\u5BA2\u6237\u7AEF\u5411\u670D\u52A1\u7AEF\u4F20\u9012\u7528\u6237\u540D\
  \u548C\u5BC6\u7801\u4EE5\u83B7\u53D6\u8D44\u6E90\u7684\u65B9\u5F0F\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u8BBF\u95EE\u53D7\u5BC6\u7801\u4FDD\u62A4\
  \u7684\u7F51\u9875\u6216API\u3002."
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

## How to: 怎么做？
PowerShell发起带基本认证的HTTP请求很简单。你需要构建一个凭据对象，然后将其与请求一起发送。

```PowerShell
# 用户名和密码
$user = 'yourUsername'
$pass = 'yourPassword'

# 将明文密码转为SecureString
$securePass = ConvertTo-SecureString $pass -AsPlainText -Force

# 创建凭据对象
$credentials = New-Object System.Management.Automation.PSCredential($user, $securePass)

# 基本认证头信息
$headers = @{
    Authorization = 'Basic ' + [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes("$user:$pass"))
}

# 发起请求
$response = Invoke-RestMethod -Uri 'http://yourapi.com/resource' -Method Get -Headers $headers -Credential $credentials

# 输出结果
$response
```

示例输出：

```PowerShell
# 假设API返回json格式的数据
{
    "data": "这里是受保护的数据"
}
```

## Deep Dive 深入探讨
历史上，基本认证因其简单性被广泛使用，但因其安全性低（密码以base64编码方式明文传输）逐渐被现代认证方式比如OAuth代替。你还可以使用`Invoke-WebRequest`而不是`Invoke-RestMethod`，如果你想要更低层次的HTTP请求控制或需要处理非RESTful的网页内容。在内部，`Invoke-RestMethod`会处理JSON或XML响应数据，让你可以很方便地访问返回对象的属性，而`Invoke-WebRequest`返回一个响应对象，你需要手动处理数据。

在实施中，最好使用HTTPS来加强安全性，因为HTTP不加密，可能会被窃听。此外，如果你在脚本中硬编码了用户名和密码，一定要确保脚本的安全性不会被应用以外的用户访问。

## See Also 参考链接
- [Invoke-RestMethod documentation - Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [PSCredential Class - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.management.automation.pscredential)
- [Basic access authentication - Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Using `Invoke-WebRequest` - Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
