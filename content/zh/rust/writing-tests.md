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

## 什么 & 为什么？

编写测试是指为代码编写可自动化的验证步骤，以确保代码的正确性和可靠性。程序员编写测试的原因是为了避免代码出现错误和故障，从而提高代码的质量和稳定性，以及节省时间和资源。

## 如何：

```Rust 
//创建一个名为 "addition" 的测试模块
mod addition {
  //导入需要测试的函数
  use super::add;

  //定义测试函数
  #[test]
  fn test_add() {
    //断言函数的结果是否符合预期
    assert_eq!(add(2, 3), 5);
  }
}

//定义需要测试的函数
fn add(a: i32, b: i32) -> i32 {
  return a + b;
}

//运行测试
$ cargo test
```

## 深入了解：

编写测试的概念始于软件测试的发展。除了编写测试，程序员也可以使用其他方式来确保代码的正确性，如手动测试或代码审查。Rust提供了内置的测试框架和断言宏来简化编写测试的过程。

## 参考链接：

- [Rust编程语言官方文档：测试](https://doc.rust-lang.org/book/testing.html) 
- [Rust By Example：测试](https://doc.rust-lang.org/rust-by-example/testing.html)