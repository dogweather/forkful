---
title:                "编写测试"
aliases:
- /zh/ruby/writing-tests/
date:                  2024-02-03T19:31:55.895533-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写测试"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?
在 Ruby 中进行测试，目的是验证代码在不同条件下是否按预期工作。程序员编写测试的目的是保证代码正确性，防止回归，并促进重构，以期望构建出健壮且可维护的应用程序。

## 如何做:
Ruby 自带一个叫做 `Test::Unit` 的内置库，用于编写单元测试，将测试实践封装在简单的结构中。然而，Ruby 社区经常倾向于使用第三方库，如 RSpec 和 Minitest，因为它们提供了增强的表达性和灵活性。

### 使用 `Test::Unit`:
```ruby
require 'test/unit'

class CalculatorTest < Test::Unit::TestCase
  def test_addition
    result = 2 + 2
    assert_equal 4, result
  end
end
```
从终端运行你的测试文件，你应该会得到一个标示测试成功或失败的输出：
```
Loaded suite test_calculator
Started
.
Finished in 0.001288 seconds.
1 tests, 1 assertions, 0 failures, 0 errors, 0 pendings, 0 omissions, 0 notifications
100% passed
```

### 使用 RSpec:
RSpec 是一个流行的 Ruby 行为驱动开发 (BDD) 框架。用 `gem install rspec` 安装这个gem，然后在你的项目中用 `rspec --init` 来初始化它。

```ruby
# calculator_spec.rb
require_relative '../calculator'

describe Calculator do
  it 'correctly adds two numbers' do
    expect(Calculator.add(2, 2)).to eq(4)
  end
end
```
用 `rspec` 命令运行测试。示例输出：
```
.

Finished in 0.002 seconds (files took 0.1 seconds to load)
1 example, 0 failures
```

### 使用 Minitest:
Minitest 提供了一套完整的测试设施，支持 TDD、BDD、模拟和基准测试。用 `gem install minitest` 安装它，并按如下方式使用：

```ruby
# test_calculator.rb
require 'minitest/autorun'
require_relative '../calculator'

class CalculatorTest < Minitest::Test
  def test_addition
    assert_equal 4, Calculator.add(2, 2)
  end
end
```

直接运行你的测试文件，或通过为 minitest 设置的 `rake` 任务运行。示例输出：
```
Run options: --seed 33407

# Running:

.

Finished in 0.001027s, 974.5922 runs/s, 974.5922 assertions/s.
1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

通过在你的 Ruby 项目中使用这些库来实现测试，你坚持最佳实践，从而导致更可靠和更易维护的代码基础。
