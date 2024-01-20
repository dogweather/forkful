---
title:                "编写测试代码"
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
写测试是创建用来验证代码功能正确性的小段程序。程序员通过测试确保代码可靠，减少BUG。

## How to: (怎么做：)
```gleam
import gleam/should
import my_module

pub fn my_test() {
  my_module.my_function(1) 
  |> should.equal(Ok(2))
}
```

输出结果：
```
test my_test ... ok
```

## Deep Dive (深入探索)
测试起源于软件工程的早期阶段。Gleam中，常用`gleam/should`库进行断言测试；此外还有`gleam/expect`。Gleam的测试遵循Erlang的标准，并与其豊富的测试生态系统兼容。

## See Also (另请参阅)
- Gleam 官方文档: [https://gleam.run/book](https://gleam.run/book)