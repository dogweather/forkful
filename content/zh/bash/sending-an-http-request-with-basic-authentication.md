---
title:                "发送基本认证的http请求"
html_title:           "Bash: 发送基本认证的http请求"
simple_title:         "发送基本认证的http请求"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么

有时，我们需要向一个需要验证的网站发送 HTTP 请求。此时，我们可以使用基本身份验证来提供用户名和密码，并获得访问权限。

## 如何

我们可以使用 curl 命令来发送带有基本身份验证的 HTTP 请求。首先，使用 ```-u``` 参数来提供用户名和密码，然后指定请求的方法、URL 和其他选项。

例如，要获取 https://example.com 网站的内容，我们可以使用以下命令：

```Bash
curl -u username:password https://example.com
```

这将返回网站的 HTML 内容，并使用提供的用户名和密码进行身份验证。

## 深入探讨

基本身份验证使用的是 HTTP Header 字段。当我们发送带有基本身份验证的 HTTP 请求时，会添加一个名为 ```Authorization: Basic``` 的字段。这个字段的值是通过将用户名和密码进行 base64 编码来生成的。

此外，如果将用户名或密码留空，那么只需要在 curl 命令中提供用户名或密码即可。例如，如果用户名为空，则可以使用以下命令：

```Bash
curl -u :password https://example.com
```

## 参考资料

- [cURL官方文档 - 发送HTTP认证请求](https://curl.haxx.se/docs/manpage.html#-u)
- [HTTP基本身份验证介绍](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication#%E5%9F%BA%E6%9C%AC%E8%AE%A4%E8%AF%81%E6%96%B9%E6%B3%95)

## 请参阅

- [使用Bash进行思考的文档撰写者](https://rickyhsuanyuanli.gitbook.io/machine-learning-notes/1.-lang)

## 注

如果您在尝试发送带有基本身份验证的 HTTP 请求时遇到问题，请确保您提供的用户名和密码是正确的，并且使用正确的编码方式。