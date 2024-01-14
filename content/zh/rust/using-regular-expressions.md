---
title:                "Rust: 使用正则表达式"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 为什么

作为一名程序员，你可能会遇到需要搜索、替换或验证文本的情况。而正则表达式就是帮助你处理这些任务的强大工具。它能够以非常灵活的方式匹配文本模式，让你节省大量的时间和精力。

## 如何

如果你想在Rust中使用正则表达式，首先需要在代码文件的开头引入 `regex` crate。然后，在需要使用正则表达式的函数中，你可以使用 `Regex::new()` 方法来创建一个正则表达式对象，如下所示：

```rust
use regex::Regex;

let re = Regex::new(r"hello ([a-z]+)").unwrap();
```

这里，我们创建了一个正则表达式来匹配以 "hello" 开头，后面跟着一个或多个小写字母的字符串。使用 `unwrap()` 方法来捕获可能的错误，并将结果存储在 `re` 变量中。

接下来，我们可以使用 `find()` 方法来搜索我们想要匹配的文本，如下所示：

```rust
let text = "hello world";
let result = re.find(text).unwrap();
```

这里，我们将 `re` 正则表达式对象应用到字符串 "hello world" 上，并将匹配结果存储在 `result` 变量中。最后，我们可以使用 `as_str()` 方法来将匹配结果转换为字符串并打印出来，如下所示：

```rust
println!("{}", result.as_str()); // prints "hello world"
```

## 深入了解

正则表达式虽然强大，但也有一些复杂的概念需要我们深入了解。例如，有时候我们需要在匹配模式中使用特殊字符，但是这些特殊字符又可能会与正则表达式语法冲突。这时，我们可以使用反斜杠 "`\`" 来转义这些字符，让它们变成普通字符。

另外，正则表达式还有一些特殊的元字符，如 `?`、`*`、`+` 等，用来表示重复次数。要注意，这些重复次数是贪婪的，也就是说它们会尽可能地匹配更多的字符。若要取消贪婪匹配，可以在重复元字符后加上一个问号 "`?`"。

最后，还有一些特殊的捕获组概念，可以让我们在匹配模式中指定具体的内容并在后续使用。然而，使用捕获组可能会有一些性能开销，因此建议在不需要后续使用的情况下尽量避免使用。

## 参考资料

- 官方Rust正则表达式文档：https://docs.rs/regex/1.4.2/regex/
- Rust正则表达式的基础教程：https://www.tutorialspoint.com/rust/rust_regular_expressions.htm
- Rust regex crate的GitHub仓库：https://github.com/rust-lang/regex

## 另请参阅

- Rust官方文档：https://www.rust-lang.org/zh-CN/learn
- Rust中文社区：https://rust.cc/
- Rust中文书籍：https://rust-lang-cn.org/survey-2019/books/