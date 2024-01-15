---
title:                "解析HTML"
html_title:           "Rust: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/parsing-html.md"
---

{{< edit_this_page >}}

## 为什么

为了构建现代网页，我们需要从HTML文件中提取出结构化的数据。熟练掌握HTML解析技术可以帮助开发者有效地处理网页数据，从而使网页更加美观、易于维护。

## 如何

要在Rust中解析HTML，我们需要使用一个强大的库，称为```scraper```。它提供了一系列功能，可让我们轻松地提取HTML文件中的数据。

首先，我们需要向我们的项目中添加```scraper```库。在```Cargo.toml```文件中，我们可以添加以下行：

```Rust
[dependencies]
scraper = "0.11.0"
```

然后，我们可以使用```scraper```库来解析HTML文件。让我们来看一个例子，假设我们有一个名为```example.html```的文件，内容如下：

```html
<html>
  <head>
    <title>示例</title>
  </head>
  <body>
    <h1>Hello World!</h1>
    <p>This is a sample HTML file.</p>
    <ul>
      <li>Item 1</li>
      <li>Item 2</li>
      <li>Item 3</li>
    </ul>
  </body>
</html>
```

现在，我们想要提取出```h1```标签内的文本。我们可以使用如下代码来完成：

```Rust
use scraper::{Html, Selector};

fn main() {
    let html = include_str!("example.html");
    let document = Html::parse_document(html);
    let selector = Selector::parse("h1").unwrap();

    for element in document.select(&selector) {
        println!("{}", element.text().collect::<String>());
    }
}
```

运行上面的代码，输出将会是：

```
Hello World!
```

我们还可以通过标签属性来提取数据。例如，如果我们想要提取```li```标签的内容，我们可以使用如下代码：

```Rust
use scraper::{Html, Selector};

fn main() {
    let html = include_str!("example.html");
    let document = Html::parse_document(html);
    let selector = Selector::parse("ul li").unwrap();

    for element in document.select(&selector) {
        println!("{}", element.text().collect::<String>());
    }
}
```

输出将会是：

```
Item 1
Item 2
Item 3
```

除了提取文本，我们还可以获取标签的属性。例如，如果我们想要获取```a```标签的链接地址，我们可以使用如下代码：

```Rust
use scraper::{Html, Selector};

fn main() {
    let html = include_str!("example.html");
    let document = Html::parse_document(html);
    let selector = Selector::parse("a").unwrap();

    for element in document.select(&selector) {
        if let Some(link) = element.value().attr("href") {
            println!("{}", link);
        }
    }
}
```

输出将会是：

```
https://example.com
```

## 深入探讨

```scraper```库提供了许多方便的功能来解析HTML文件。它使用了类似于CSS选择器的语法，使得选择和提取特定标签和属性变得更加简单。同时，它还能够处理错误的HTML格式，使得解析不会因为格式问题出错。

但是，需要注意的是，```scraper```库并不是最快的HTML解析库。如果性能是你关心的重点，那么可以尝试使用其他库，如```html5ever```或```select.rs```。

## 查看更多

- [scraper库文档](https://docs.rs/scraper/0.11.0/scraper/)
- [Rust官方文档](https://www.rust-lang.org/zh-CN/)
- [Rust编程语言论坛](https://users.rust-lang.org/)