---
title:                "发出 HTTP 请求"
date:                  2024-01-20T17:59:46.758805-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (什么及为什么？)
发送HTTP请求，就是向网络服务器发信息，等回应。程序员这么做，主要是为了和网上服务交换数据。

## How to: (如何操作：)
在Fish Shell中，发送HTTP请求可以用`curl`命令。下面是代码示例：

```Fish Shell
curl http://example.com
```

收到的输出可能是这样的：

```html
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

还可以用`httpie`，首先要安装：

```Fish Shell
sudo apt install httpie
```

然后发送请求：

```Fish Shell
http http://example.com
```

输出会这样显示：

```http
HTTP/1.1 200 OK
...

<!doctype html>
...
```

## Deep Dive (深入探讨)
早期，发送HTTP请求主要依靠底层编程或使用命令行工具，如telnet。`curl`是一个长青的命令，自1997年以来广泛使用，支持多种协议。

替代工具包括`wget`和现代的`http`, `httpie`更人性化，输出格式也更清晰。在Fish中使用它们，就是向你展示的一样，不复杂。

身为Fish用户，你得知道，Fish有它自己的语法规则。和bash、zsh不同，Fish着重于简化和高效。例如，Fish不需要像其他shell那样使用`;`来分隔命令。

## See Also (另请参阅)
- Fish Shell官网: [fishshell.com](https://fishshell.com)
- `curl`说明文档: [curl.se/docs](https://curl.se/docs/)
- `httpie`官方网站: [httpie.io](https://httpie.io)
- HTTP协议入门: [MDN HTTP overview](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)