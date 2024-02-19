---
aliases:
- /zh/rust/working-with-xml/
date: 2024-01-26 04:36:09.565903-07:00
description: "XML\uFF0C\u5373\u53EF\u6269\u5C55\u6807\u8BB0\u8BED\u8A00\uFF08eXtensible\
  \ Markup Language\uFF09\uFF0C\u5C31\u50CF\u662FJSON\u90A3\u4E2A\u5197\u957F\u7684\
  \u5802\u5144\u3002\u5F53\u5904\u7406\u9057\u7559\u7CFB\u7EDF\u3001\u4F01\u4E1A\u8F6F\
  \u4EF6\u6216\u672A\u8DDF\u968FJSON\u6F6E\u6D41\u7684API\u65F6\uFF0C\u4F60\u5C06\u4E0D\
  \u5F97\u4E0D\u4E0EXML\u6253\u4EA4\u9053\u3002\u5728\u9700\u8981\u6570\u636E\u4EA4\
  \u6362\u7684\u5730\u65B9\uFF0CXML\u4F9D\u7136\u7A33\u56FA\u5730\u7AD9\u7740\u3002"
lastmod: 2024-02-18 23:08:58.962984
model: gpt-4-0125-preview
summary: "XML\uFF0C\u5373\u53EF\u6269\u5C55\u6807\u8BB0\u8BED\u8A00\uFF08eXtensible\
  \ Markup Language\uFF09\uFF0C\u5C31\u50CF\u662FJSON\u90A3\u4E2A\u5197\u957F\u7684\
  \u5802\u5144\u3002\u5F53\u5904\u7406\u9057\u7559\u7CFB\u7EDF\u3001\u4F01\u4E1A\u8F6F\
  \u4EF6\u6216\u672A\u8DDF\u968FJSON\u6F6E\u6D41\u7684API\u65F6\uFF0C\u4F60\u5C06\u4E0D\
  \u5F97\u4E0D\u4E0EXML\u6253\u4EA4\u9053\u3002\u5728\u9700\u8981\u6570\u636E\u4EA4\
  \u6362\u7684\u5730\u65B9\uFF0CXML\u4F9D\u7136\u7A33\u56FA\u5730\u7AD9\u7740\u3002"
title: "\u5904\u7406XML"
---

{{< edit_this_page >}}

## 什么 & 为什么？
XML，即可扩展标记语言（eXtensible Markup Language），就像是JSON那个冗长的堂兄。当处理遗留系统、企业软件或未跟随JSON潮流的API时，你将不得不与XML打交道。在需要数据交换的地方，XML依然稳固地站着。

## 如何操作：
在Rust中，你可以使用像`xml-rs`这样的crate来处理XML。通过将`xml-rs = "0.8"`添加到你的`Cargo.toml`来安装。下面是解析一个简单XML的方法：

```rust
extern crate xml;

use xml::reader::{EventReader, XmlEvent};

fn main() {
    let xml_data = r#"<book category="fiction">
    <title>Rust in Action</title>
    <author>Tim McNamara</author>
    <year>2021</year>
</book>"#;

    let parser = EventReader::from_str(xml_data);
    for e in parser {
        match e {
            Ok(XmlEvent::StartElement { name, .. }) => {
                println!("开始: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("文本: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("结束: {}", name);
            }
            Err(e) => {
                println!("错误: {}", e);
            }
            _ => {}
        }
    }
}
```

输出：
```
开始: book
开始: title
文本: Rust in Action
结束: title
开始: author
文本: Tim McNamara
结束: author
开始: year
文本: 2021
结束: year
结束: book
```
这段代码通过流式读取XML，处理开始和结束元素以及文本数据，记录每一步。

## 深入探讨：
XML在技术领域算是资历较深的，它是在90年代末为网络设计的。它的设计强调可读性（对机器和人类都是）以及广泛的自我描述数据。

有替代方案吗？当然，JSON是现代网络API的首选，更轻量且噪音更少。与此同时，YAML因其清晰的布局而赢得了配置文件的青睐。但XML不会很快消失——大量基础设施是建立在它的基础上的。

Rust处理XML的底层借助了迭代器模式，保持低内存使用和高性能。你会发现像`serde-xml-rs`这样的crate提供了类似serde的体验——这对于那些习惯处理JSON的人来说是个好消息。

## 另请参阅：
有关Rust和XML的更多信息：
- `serde-xml-rs` 适用于Rust的serde兼容性：[https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- 官方Rust文档（因为复习永远不是坏事）：[https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
