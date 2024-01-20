---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

下载网页是获取网络上一个页面的内容并将其保存在本地的过程。程序员下载网页是为了进行网页数据的分析，或为离线使用提供访问能力。

## 操作步骤:

在Rust中，我们可以使用`reqwest`库来下载网页。首先在您的`Cargo.toml` 文件中添加库：

```Rust
[dependencies]
reqwest = { version = "0.11", features = ["blocking"] }
```

然后在您的代码中添加以下代码：

```Rust
use std::io::Read;
use reqwest::blocking::get;

fn main() {
  let mut res = get("http://example.com").unwrap();
  let mut body = String::new();
  res.read_to_string(&mut body).unwrap();
  println!("网页的内容: {}", body);
}
```

运行此代码，它将打印出网站的内容。

## 深入挖掘:

下载网页这个概念在互联网的早期就已经存在了，早期使用电话拨号方式访问因特网的用户会通过下载网页以便离线查看。这样可以节省在线时间，降低上网成本。

在下载网页时，除了使用上述的`reqwest`库外，我们还可以使用其他库，如`hyper` 。不过`reqwest`在易用性和功能性方面做了大量的优化，因此它是我们的首选。

下载操作实际上是发送了一个HTTP GET请求到特定URL，然后保存返回的内容。`reqwest`库在后台为我们处理了所有细节，包括建立连接、发送请求、接收响应和关闭连接。这也是为什么我们需要为`reqwest`启用 "blocking" 特性，因为下载网页是一个阻塞操作。

## 另请参阅:

对于Rust中具体的网络编程和HTTP请求处理，您可以访问以下链接：

1. 官方文档：[Rust reqwest文档](https://docs.rs/reqwest/0.11.3/reqwest/)
3. 相关资讯：[Rust 完整指南](https://learnku.com/docs/rust-lang/2020/ch17-01-what-is-async-io/7962)