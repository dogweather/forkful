---
title:                "解析HTML"
html_title:           "Clojure: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/parsing-html.md"
---

{{< edit_this_page >}}

# 什么和为什么?
解析HTML是识别和理解HTML文件内容的过程。程序员做这件事是因为它能帮助他们从HTML文档中提取数据，理解并利用其结构。

# 如何解析：

Rust中解析HTML的一个很好的库叫做scraper。首先，添加如下依赖到你的 Cargo.toml 파일：

```rust
[dependencies]
scraper = "0.3.0"
```

然后，用下列代码来解析HTML： 

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html = r#"<p class="foo">Hello, world!</p>"#;
    let parsed_html = Html::parse_document(html);

    let foo = Selector::parse(".foo").unwrap();
    for element in parsed_html.select(&foo) {
        let text = element.text().collect::<Vec<_>>();
        println!("{}", text.concat());
    }
}
```

运行这段代码，会在控制台上输出"Hello, world!"

# 深入了解：

HTML解析源于网页的历史。早在1990年，最初的网络浏览器就开始解析HTML。

有许多方法可以解析HTML，而不仅仅是用Rust和scraper库。其他编程语言也有许多库和工具可以用来解析HTML，如Python的BeautifulSoup，JavaScript的JSDOM等。

使用scraper库解析HTML并不复杂。该库首先将HTML文档转换成DOM（文档对象模型），然后通过选择器来查找并提取你想要的信息。

# 另请参阅：

2. [Rust Scraper库文档](https://docs.rs/scraper/0.12.0/scraper/)