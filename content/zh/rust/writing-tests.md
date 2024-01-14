---
title:    "Rust: 写作测试"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/rust/writing-tests.md"
---

{{< edit_this_page >}}

##为什么

编写测试在Rust中是一个重要的实践，可以帮助我们确保代码的质量和稳定性。它可以帮助我们发现潜在的错误或问题，从而保证我们的代码能够正常运行并且符合预期。

##如何编写测试

编写测试的第一步是使用标准库中的`#[test]`宏来标识测试函数。接着，我们可以使用assert宏来验证我们的代码是否按预期工作，如下所示：

```Rust
#[test]
fn test_addition() {
    let result = 2 + 2;
    assert_eq!(result, 4);
}
```

在上面的代码中，我们首先定义了一个测试函数`test_addition()`，然后调用了标准库中的assert宏来验证`2+2`是否等于4。如果测试失败，则会抛出一个panic，表示测试不通过。接下来，我们可以通过运行`cargo test`命令来运行所有的测试。

除了基本的单元测试外，我们还可以编写集成测试来测试多个模块或组件之间的交互。我们可以将集成测试放在`tests`目录中，然后使用`cargo test --integration`来运行它们。完整的测试代码示例如下所示：

```Rust
fn add(n1: i32, n2: i32) -> i32 {
    n1 + n2
}

#[test]
fn test_addition() {
    let result = add(2, 2);
    assert_eq!(result, 4);
}

#[cfg(test)]
mod integration_tests {
    #[test]
    fn test_addition() {
        let result = crate::add(2, 2);
        assert_eq!(result, 4);
    }
}
```

##深入了解测试

除了上述介绍的基本的测试之外，Rust还提供了更多高级的测试功能，例如属性测试、代码覆盖率统计等。我们可以通过在测试函数前添加`#[should_panic]`属性来测试代码是否能够正确地处理panic，或者使用`#[ignore]`属性来跳过某些测试。我们还可以通过设置`RUSTFLAGS`环境变量来打开代码覆盖率的统计功能，从而了解我们的测试覆盖了多少代码。更多关于测试的细节和高级功能可以参考Rust官方文档。

##参考链接

- [Rust官方文档：测试](https://doc.rust-lang.org/stable/book/ch11-00-testing.html)
- [Rust官方文档：属性测试](https://doc.rust-lang.org/stable/book/ch11-02-running-tests.html#the-tests-directory)
- [Rust官方文档：代码覆盖率](https://doc.rust-lang.org/stable/book/ch11-03-test-organization.html#showing-function-paths-being-tested)