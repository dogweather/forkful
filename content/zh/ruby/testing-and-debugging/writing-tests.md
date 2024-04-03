---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:55.895533-07:00
description: "\u5982\u4F55\u505A: Ruby \u81EA\u5E26\u4E00\u4E2A\u53EB\u505A `Test::Unit`\
  \ \u7684\u5185\u7F6E\u5E93\uFF0C\u7528\u4E8E\u7F16\u5199\u5355\u5143\u6D4B\u8BD5\
  \uFF0C\u5C06\u6D4B\u8BD5\u5B9E\u8DF5\u5C01\u88C5\u5728\u7B80\u5355\u7684\u7ED3\u6784\
  \u4E2D\u3002\u7136\u800C\uFF0CRuby \u793E\u533A\u7ECF\u5E38\u503E\u5411\u4E8E\u4F7F\
  \u7528\u7B2C\u4E09\u65B9\u5E93\uFF0C\u5982 RSpec \u548C Minitest\uFF0C\u56E0\u4E3A\
  \u5B83\u4EEC\u63D0\u4F9B\u4E86\u589E\u5F3A\u7684\u8868\u8FBE\u6027\u548C\u7075\u6D3B\
  \u6027\u3002 #."
lastmod: '2024-03-13T22:44:48.377149-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u81EA\u5E26\u4E00\u4E2A\u53EB\u505A `Test::Unit` \u7684\u5185\u7F6E\
  \u5E93\uFF0C\u7528\u4E8E\u7F16\u5199\u5355\u5143\u6D4B\u8BD5\uFF0C\u5C06\u6D4B\u8BD5\
  \u5B9E\u8DF5\u5C01\u88C5\u5728\u7B80\u5355\u7684\u7ED3\u6784\u4E2D\u3002\u7136\u800C\
  \uFF0CRuby \u793E\u533A\u7ECF\u5E38\u503E\u5411\u4E8E\u4F7F\u7528\u7B2C\u4E09\u65B9\
  \u5E93\uFF0C\u5982 RSpec \u548C Minitest\uFF0C\u56E0\u4E3A\u5B83\u4EEC\u63D0\u4F9B\
  \u4E86\u589E\u5F3A\u7684\u8868\u8FBE\u6027\u548C\u7075\u6D3B\u6027."
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

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
