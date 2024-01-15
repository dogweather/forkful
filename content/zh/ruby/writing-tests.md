---
title:                "编写测试"
html_title:           "Ruby: 编写测试"
simple_title:         "编写测试"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么

为什么在编写代码时需要添加测试？很简单，这可以确保代码的质量和功能的正确性。测试可以帮助开发者在修改代码后快速检查是否引入了新的错误，并且确保旧的代码仍然正常工作。

## 如何做

编写测试的第一步是在代码中引入RSpec测试框架。下面是一个简单的示例：

```ruby
class Calculator
  def add(a, b)
    return a + b
  end

  def subtract(a, b)
    return a - b
  end
end

describe "Calculator" do
  let(:calculator) {Calculator.new}

  it "adds two numbers" do
    expect(calculator.add(2, 5)).to eq(7)
  end

  it "subtracts two numbers" do
    expect(calculator.subtract(10, 3)).to eq(7)
  end
end
```
代码定义了一个计算器类，其中包含两个函数用于加法和减法。测试用例使用`expect`关键字来断言函数的返回值是否符合预期。运行测试可以得到以下输出：

```
Finished in 0.00153 seconds (files took 0.074 seconds to load)
2 examples, 0 failures
```

这说明两个测试用例都通过了，也就是说我们的代码无错误。

## 深入了解

写好的测试应该涵盖代码的各种情况，包括异常情况和边界条件。测试覆盖率也是评估测试质量的一个重要指标。代码中若有分支语句，测试应该覆盖每一种可能的情况。

此外，测试代码应该具有可读性和可维护性，遵循编码规范和最佳实践。一些常见的测试方法包括TDD（测试驱动开发）和BDD（行为驱动开发），可以帮助开发者更有效地编写测试。

## 查看更多

- [RSpec文档](https://rspec.info/documentation/)
- [TDD和BDD的区别](https://medium.com/@tofra1999/test-driven-development-tdd-%E6%98%AF%E4%BB%80%E4%B9%88-918a156e65a5)
- [编写有效的测试用例](https://www.ibm.com/developerworks/cn/linux/l-cn-test/index.html)