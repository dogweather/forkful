---
title:                "下载网页"
date:                  2024-01-20T17:43:54.486958-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
下载网页意味着从互联网上获取网页的内容。程序员这么做主要是为了处理数据、网页抓取或在线应用集成。

## How to: 如何做：
```Gleam
import gleam/http
import gleam/io

pub fn main() {
  case http.get("https://example.com") {
    Ok(response) -> io.println(response.body)
    Error(error) -> io.println(error)
  }
}
```

示例输出：
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive 深入探究
在早期，下载网页通常使用命令行工具如 `curl` 或编程库比如 Python 的 `requests`。Gleam 提供了更现代、类型安全的方法访问网络资源。替代方案包括直接使用 Erlang 的 `httpc` 模块或其他语言的库。Gleam 的 `http` 模块底层实际上是封装了 `httpc`，但提供了更友好的类型安全接口。

## See Also 另请参阅
- [Gleam HTTP documentation](https://hexdocs.pm/gleam_http/)
- [`httpc` Erlang documentation](http://erlang.org/doc/man/httpc.html)
- [MDN Web Docs on HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
