---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:55.895533-07:00
description: "\u5728 Ruby \u4E2D\u8FDB\u884C\u6D4B\u8BD5\uFF0C\u76EE\u7684\u662F\u9A8C\
  \u8BC1\u4EE3\u7801\u5728\u4E0D\u540C\u6761\u4EF6\u4E0B\u662F\u5426\u6309\u9884\u671F\
  \u5DE5\u4F5C\u3002\u7A0B\u5E8F\u5458\u7F16\u5199\u6D4B\u8BD5\u7684\u76EE\u7684\u662F\
  \u4FDD\u8BC1\u4EE3\u7801\u6B63\u786E\u6027\uFF0C\u9632\u6B62\u56DE\u5F52\uFF0C\u5E76\
  \u4FC3\u8FDB\u91CD\u6784\uFF0C\u4EE5\u671F\u671B\u6784\u5EFA\u51FA\u5065\u58EE\u4E14\
  \u53EF\u7EF4\u62A4\u7684\u5E94\u7528\u7A0B\u5E8F\u3002"
lastmod: '2024-03-11T00:14:22.189523-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Ruby \u4E2D\u8FDB\u884C\u6D4B\u8BD5\uFF0C\u76EE\u7684\u662F\u9A8C\
  \u8BC1\u4EE3\u7801\u5728\u4E0D\u540C\u6761\u4EF6\u4E0B\u662F\u5426\u6309\u9884\u671F\
  \u5DE5\u4F5C\u3002\u7A0B\u5E8F\u5458\u7F16\u5199\u6D4B\u8BD5\u7684\u76EE\u7684\u662F\
  \u4FDD\u8BC1\u4EE3\u7801\u6B63\u786E\u6027\uFF0C\u9632\u6B62\u56DE\u5F52\uFF0C\u5E76\
  \u4FC3\u8FDB\u91CD\u6784\uFF0C\u4EE5\u671F\u671B\u6784\u5EFA\u51FA\u5065\u58EE\u4E14\
  \u53EF\u7EF4\u62A4\u7684\u5E94\u7528\u7A0B\u5E8F\u3002"
title: "\u7F16\u5199\u6D4B\u8BD5"
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
