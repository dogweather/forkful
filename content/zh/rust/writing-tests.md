---
title:                "编写测试"
html_title:           "Rust: 编写测试"
simple_title:         "编写测试"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么

写测试是软件开发中的一个重要步骤。通过编写测试，我们可以确保代码的质量，减少潜在的错误和bug，同时也可以提高代码的可维护性。因此，写测试能够很大程度上提高软件开发的效率和质量。

## 如何做

在Rust中，编写测试非常简单。首先，在项目根目录下创建名为`tests`的文件夹，并在其中创建一个用于测试的源文件，命名为`tests.rs`。然后，在该文件中使用`#[cfg(test)]`定义一个单元测试模块，接着就可以进行测试了。下面是一个简单的示例：

```Rust
#[cfg(test)]
mod tests {
    #[test]
    fn test_addition() {
        assert_eq!(2 + 2, 4);
    }
}
```

在这个例子中，我们定义了一个名称为`test_addition`的单元测试函数，在函数体中使用`assert_eq!`宏来断言测试结果。接着，我们可以通过在终端中使用`cargo test`命令来运行所有的测试，或者使用`cargo test test_addition`来运行指定的测试。

## 深入挖掘

除了单元测试外，Rust还提供了集成测试的功能，用于测试多个模块之间的交互。集成测试可以在项目根目录下的`tests`文件夹中编写，与单元测试不同的是，需要使用`#[test]`标注来定义测试函数。另外，在`src`文件夹中的任何地方编写的测试都将被自动识别为集成测试。

此外，Rust还支持`#[ignore]`标注来忽略某些测试，以及`#[should_panic]`标注来测试代码是否按预期产生panic。更多关于测试的信息，我们可以查看Rust官方文档或其他博客文章。

## 参考文献

- Rust官方文档：https://doc.rust-lang.org/book/ch11-01-writing-tests.html
- Rust测试教程：https://doc.rust-lang.org/book/ch11-02-running-tests.html
- 关于单元测试与集成测试的区别：https://stackoverflow.com/questions/49049664/what-is-the-difference-between-unit-tests-and-integration-tests