---
title:                "使用基本验证发送http请求"
html_title:           "Go: 使用基本验证发送http请求"
simple_title:         "使用基本验证发送http请求"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 何为什么？

发送带基本认证的HTTP请求是指在发送HTTP请求时，附带一个用户名和密码以进行身份验证。程序员之所以这样做是为了保护敏感的API或资源，确保只有经过认证的用户才能访问。

# 如何：

```Go
// 导入必要的包
import (
	"fmt"
	"net/http"
	"bytes"
)

// 创建一个基本认证的HTTP请求
req, err := http.NewRequest("GET", "https://example.com/api", nil)
if err != nil {
    fmt.Println(err)
}

// 添加认证头
req.SetBasicAuth("username", "password")

// 发送请求并获取响应
res, err := http.DefaultClient.Do(req)
if err != nil {
    fmt.Println(err)
}

// 输出响应的状态码
fmt.Println(res.Status)
```

# 深入探讨：

1. 历史背景：基本认证是HTTP协议早期版本的一部分，现已被认为不安全，并被摒弃。
2. 其他选择：现在更常用的替代方式是OAuth认证。
3. 实现细节：基本认证是通过在请求头中加入一个包含用户名和密码的Base64编码字符串来实现的。

# 参考链接：

- https://golang.org/pkg/net/http/#Request.SetBasicAuth
- https://www.oauth.com/oauth1/
- https://www.rfc-editor.org/rfc/rfc7617.txt