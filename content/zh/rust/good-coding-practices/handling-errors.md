---
date: 2024-01-26 00:57:21.237992-07:00
description: "\u9519\u8BEF\u5904\u7406\u662F\u6307\u5F53\u4E8B\u60C5\u51FA\u73B0\u95EE\
  \u9898\u65F6\u7684\u5E94\u5BF9\u7B56\u7565\u3002\u7F16\u7A0B\u4EBA\u5458\u8FDB\u884C\
  \u9519\u8BEF\u5904\u7406\uFF0C\u4EE5\u5E94\u5BF9\u610F\u5916\u60C5\u51B5\uFF0C\u786E\
  \u4FDD\u4ED6\u4EEC\u7684Rust\u7A0B\u5E8F\u5728\u9047\u5230\u5C0F\u95EE\u9898\u65F6\
  \u4E0D\u4F1A\u5D29\u6E83\u3002"
lastmod: '2024-03-13T22:44:47.529120-06:00'
model: gpt-4-1106-preview
summary: "\u9519\u8BEF\u5904\u7406\u662F\u6307\u5F53\u4E8B\u60C5\u51FA\u73B0\u95EE\
  \u9898\u65F6\u7684\u5E94\u5BF9\u7B56\u7565\u3002\u7F16\u7A0B\u4EBA\u5458\u8FDB\u884C\
  \u9519\u8BEF\u5904\u7406\uFF0C\u4EE5\u5E94\u5BF9\u610F\u5916\u60C5\u51B5\uFF0C\u786E\
  \u4FDD\u4ED6\u4EEC\u7684Rust\u7A0B\u5E8F\u5728\u9047\u5230\u5C0F\u95EE\u9898\u65F6\
  \u4E0D\u4F1A\u5D29\u6E83\u3002"
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

## 是什么 & 为什么？

错误处理是指当事情出现问题时的应对策略。编程人员进行错误处理，以应对意外情况，确保他们的Rust程序在遇到小问题时不会崩溃。

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
