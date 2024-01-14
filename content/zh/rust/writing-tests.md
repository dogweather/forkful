---
title:                "Rust: 编写测试"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/writing-tests.md"
---

{{< edit_this_page >}}

为什么：写测试的原因是确保代码的质量和可靠性。通过编写测试，您可以在修改代码时及时发现问题，并保证代码的正确性。

如何做： Rust中的测试由两部分组成：单元测试和集成测试。单元测试是针对代码中的单个函数或方法进行的局部测试，而集成测试是测试多个组件之间的交互是否正常。下面是一个简单的示例，展示如何在Rust中编写单元测试和集成测试。

```Rust
// 单元测试
#[cfg(test)] // 需要放在函数之前
mod tests { // 创建一个测试模块
    // 导入需要测试的函数或方法
    use crate::math::add; 
    
    // "test"宏用于标记测试函数
    #[test]
    fn test_add() {
        // 断言函数输出是否等于预期值
        assert_eq!(add(2, 3), 5);
    }
}

// 集成测试
// 需要在项目目录下创建tests目录，并将测试文件命名为integration_test.rs
#[test]
fn test_integration() {
    // 导入需要测试的函数或方法
    use crate::math::multiply;
    
    // 断言函数输出是否等于预期值
    assert_eq!(multiply(2, 3), 6);
}
```

深入了解：除了单元测试和集成测试，Rust还支持属性测试（property-based testing）和文档测试（doc testing）。属性测试可以通过生成随机输入数据来测试函数的性质，从而更全面地验证代码的正确性。文档测试可以直接在函数的注释中写下测试用例，并通过运行 `cargo test` 来执行测试。这些测试工具可以帮助开发者更加灵活地编写测试，从而提高代码的质量。

另外，Rust社区还提供了许多优秀的测试框架和工具，如`assert`、`quickcheck`和`mockito`等，开发者可以根据自己的需求选择适合的工具来编写测试。

看看这些资源，了解更多Rust测试的内容吧！

## 参考链接：

- [Rust语言官方文档：测试](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Rust中文社区：测试](https://rustcc.cn/article?id=94806b50-1eb4-46b5-ae80-a2324f1aa874)
- [Property-based testing for Rust using quickcheck](https://github.com/BurntSushi/quickcheck)
- [Mock objects for Rust](https://github.com/lipanski/mockito)

## 参考链接：

- [Rust语言官方文档：测试](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Rust中文社区：测试](https://rustcc.cn/article?id=94806b50-1eb4-46b5-ae80-a2324f1aa874)
- [Property-based testing for Rust using quickcheck](https://github.com/BurntSushi/quickcheck)
- [Mock objects for Rust](https://github.com/lipanski/mockito)