---
date: 2024-01-26 00:57:21.237992-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Rust\u6709\u4E24\u79CD\u4E3B\u8981\u7684\
  \u9519\u8BEF\u5904\u7406\u65B9\u5F0F\uFF1A\u53EF\u6062\u590D\u9519\u8BEF\u548C\u4E0D\
  \u53EF\u6062\u590D\u9519\u8BEF\u3002\u8BA9\u6211\u4EEC\u6765\u770B\u770B\u8FD9\u4E24\
  \u79CD\u65B9\u5F0F\u3002 \u53EF\u6062\u590D\u9519\u8BEF\u4F7F\u7528`Result<T, E>`."
lastmod: '2024-04-05T22:38:46.690081-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Rust\u6709\u4E24\u79CD\u4E3B\u8981\u7684\u9519\
  \u8BEF\u5904\u7406\u65B9\u5F0F\uFF1A\u53EF\u6062\u590D\u9519\u8BEF\u548C\u4E0D\u53EF\
  \u6062\u590D\u9519\u8BEF\u3002\u8BA9\u6211\u4EEC\u6765\u770B\u770B\u8FD9\u4E24\u79CD\
  \u65B9\u5F0F\u3002 \u53EF\u6062\u590D\u9519\u8BEF\u4F7F\u7528`Result<T, E>`."
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

## 如何操作：
Rust有两种主要的错误处理方式：可恢复错误和不可恢复错误。让我们来看看这两种方式。

可恢复错误使用`Result<T, E>`:

```Rust
use std::fs::File;

fn open_file(filename: &str) -> Result<File, std::io::Error> {
    let f = File::open(filename);
    
    match f {
        Ok(file) => Ok(file),
        Err(e) => Err(e),
    }
}

fn main() {
    match open_file("hello.txt") {
        Ok(_file) => println!("文件成功打开。"),
        Err(_e) => println!("文件打开失败。"),
    }
}
```

输出可能是 "文件成功打开。" 或者 "文件打开失败。"，这取决于你的`hello.txt`。

对于不可恢复的错误，我们使用`panic!`:

```Rust
fn main() {
    // 这将导致程序因为文件可能不存在而恐慌。
    let _f = File::open("nowhere.txt").unwrap();
}
```

运行它，你将看到一条恐慌信息。你的程序立刻停止。

## 深入分析
从历史上看，编程中的错误处理一直是一个混乱的问题。Rust通过在可恢复和不可恢复错误之间进行明确区分，正确处理了这一问题。

`Result`枚举用于可恢复错误。它很明确 —— 你处理`Ok`或`Err`变体。你也有诸如`unwrap()`和`expect()`这样的方法，但它们是快速且简单的捷径，可能会导致`panic!`。

`panic!`是Rust表明出现了非常糟糕的情况且无法处理的方式。它像一个停止执行的不可恢复的错误。Rust中的恐慌通常出现在你不期望处理的错误中，例如数组越界索引。

在预期要处理错误的时候，通过返回`Result`进行错误处理是首选。这是惯用的Rust，意味着是Rust开发者约定俗成的做法。还有`Option<T>`，用于错误仅仅是某些东西变成`None`而不是`Some(T)`的情况。这一切都是为了从容应对意料之外的情况。

还有别的选择吗？当然，你可以使用其他错误处理库来获取更多的功能或更舒适的使用体验。就像`anyhow`适用于简单的错误处理，或者`thiserror`适用于库代码中的错误。

## 另请参阅
有兴趣深入了解吗？以下是你可以访问的资源：

- [Rust书籍关于错误处理](https://doc.rust-lang.org/book/ch09-00-error-handling.html) - 理解Rust错误处理哲学的好地方。
- [Rust示例：错误处理](https://doc.rust-lang.org/rust-by-example/error.html) - 互动示例，让你动手实践。

记住，良好的错误处理不仅仅是编程；它也是对你的代码用户的关怀。编码快乐！
