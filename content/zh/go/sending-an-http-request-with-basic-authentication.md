---
title:                "使用基本身份验证发送http请求"
html_title:           "Go: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

#为什么
为什么会有人使用基本身份验证发送HTTP请求？一般来说，基本身份验证是一种简单但有效的方式来验证用户的身份，以便在用户访问受保护的资源时提供一定的安全性。

##如何进行
为了发送带有基本身份验证的HTTP请求，我们需要首先创建一个http.Client对象，然后使用它来发送请求。我们可以通过设置http.Client的Transport字段来添加我们需要的身份验证信息。

例如，在下面的代码示例中，我们使用用户名和密码来进行基本身份验证，并向某个URL发送GET请求。注意，我们还需要使用encoding/base64包来进行编码。

```Go
package main

import (
	"encoding/base64"
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	username := "myusername"
	password := "mypassword"

	url := "https://example.com/api/mypath"
	req, _ := http.NewRequest("GET", url, nil)

	auth := username + ":" + password
	basicAuth := "Basic " + base64.StdEncoding.EncodeToString([]byte(auth))
	req.Header.Set("Authorization", basicAuth)

	client := &http.Client{}
	resp, _ := client.Do(req)

	body, _ := ioutil.ReadAll(resp.Body)
	fmt.Print(string(body))
}
```

以上代码的输出应该类似于以下内容：

```
{"message": "You have been successfully authenticated."}
```

##深入了解
进行基本身份验证时，用户名和密码通常是以明文的形式传递给服务器的。因此，如果你正在使用非加密的HTTP协议，建议只在安全的环境下使用基本身份验证。同时，基本身份验证也不是最安全的身份验证方式，因为它无法防止信息被窃取。因此，在传输敏感信息时，最好使用其他更安全的身份验证方式。

#另请参阅
- [Package http - The Go programming language](https://golang.org/pkg/net/http/)
- [Basic authentication - Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [How does HTTP basic authentication work? - StackOverflow](https://stackoverflow.com/questions/9534602/how-does-http-basic-authentication-work)