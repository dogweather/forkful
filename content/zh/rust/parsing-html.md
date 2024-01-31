---
title:                "解析HTML"
date:                  2024-01-20T15:33:53.417419-07:00
simple_title:         "解析HTML"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么？
解析HTML是指将HTML文本转换成可由程序理解的数据结构。程序员这么做是为了从网页中抓取数据，或操作和生成动态HTML内容。

## How to: 怎么做？
在Rust中，我们可以使用`html5ever`这个库来解析HTML。以下是如何使用的一个简单示例。

```Rust
extern crate html5ever;

use html5ever::{parse_document};
use html5ever::rcdom::{RcDom, Handle};
use html5ever::tendril::TendrilSink;

fn walk(node: &Handle) {
    // 基于节点类型进行逻辑处理
}

fn main() {
    let html_content = r#"
        <!DOCTYPE html>
        <html>
        <head>
            <title>This is a title.</title>
        </head>
        <body>
            <h1>Hello, world!</h1>
        </body>
        </html>
    "#;

    let dom = parse_document(RcDom::default(), Default::default())
                  .from_utf8()
                  .read_from(&mut html_content.as_bytes())
                  .unwrap();

    // 遍历DOM树
    walk(&dom.document);
}
```

这段代码没有输出。它只是演示了如何构建DOM树。

## Deep Dive 深入研究
HTML解析的历史很长。最初，HTML解析器必须处理极其不规则的标记，而现代解析器则必须严格遵守HTML5规范。Rust语言中的`html5ever`库就是为了高效和安全地解析HTML5而设计的。其他解析HTML的替代库包括`scraper`和`select.rs`，它们提供了类似于jQuery的选择器操作。`html5ever`的实现细节涉及到复杂的状态机和解析算法，但这可以让Rust程序员高效地处理HTML文档。

## See Also 另请参阅
- [html5ever repository](https://github.com/servo/html5ever)
