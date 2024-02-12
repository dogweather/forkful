---
title:                "下载网页"
date:                  2024-01-20T17:44:46.835751-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

下载网页就是从互联网上获取网页内容。程序员这样做可以处理、分析网页数据或者做网页备份。

## How to: (如何操作：)

首先，要使用Rust下载网页，你需要一个客户端。这里以`reqwest`库为例。如果你没有`reqwest`，需要先在Cargo.toml里加上：

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

然后，你可以用以下代码下载一个网页：

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let url = "http://example.com";
    let res = reqwest::get(url).await?;

    println!("状态码: {}", res.status());
    let body = res.text().await?;
    println!("网页内容: {}", body);

    Ok(())
}
```

运行这段代码，你会看到状态码和网页内容。

## Deep Dive (深入探讨)

在早期，Rust使用的库和方法与现在有所不同。例如，曾经流行的`hyper`库需要手动管理底层TCP连接。`reqwest`的出现简化了这个过程。

除了`reqwest`，还有其他库，如`surf`和`hyper`。每个库在性能、易用性和功能上有所不同。

深入实现细节时，你会发现`reqwest`内部使用`hyper`来处理HTTP请求，同时也利用`tokio`来异步执行。Rust通过这些库提供强大的异步支持，进而高效地处理网络请求。

## See Also (另请参阅)

- 官方reqwest文档：[https://docs.rs/reqwest](https://docs.rs/reqwest)
- Rust异步编程指南：[https://rust-lang.github.io/async-book/](https://rust-lang.github.io/async-book/)
- tokio库文档：[https://docs.rs/tokio](https://docs.rs/tokio)
