---
title:                "编写测试代码"
date:                  2024-01-19
simple_title:         "编写测试代码"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么?)
测试编写是用来检验代码正确性的实践。程序员进行测试是为了减少bug，提高代码质量，确保软件行为符合预期。

## How to: (如何操作：)
```Ruby
# 使用Ruby标准库中的Minitest进行单元测试

require 'minitest/autorun'

class Calculator
  def add(a, b)
    a + b
  end
end

class CalculatorTest < Minitest::Test
  def setup
    @calculator = Calculator.new
  end

  def test_addition
    assert_equal 5, @calculator.add(2, 3)
  end
end
```

运行上述测试，输出结果应该类似于：

```plaintext
Run options: --seed 12345

# Running:

.

Finished in 0.001025s, 975.6098 runs/s, 975.6098 assertions/s.

1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

## Deep Dive (深入探究)
历史上，Ruby测试一度主要依赖Test::Unit框架。现在，Minitest和RSpec是Ruby社区最流行的测试库。Minitest是小巧、快速的测试库，强调简洁性；RSpec则是行为驱动开发(BDD)的实现，专注于人类可读的测试描述。写测试时要考虑的细节包括测试不同的边界情况，使用mock和stub处理外部依赖，以及持续集成(CI)的应用。

## See Also (另见)
- [RSpec website](https://rspec.info/)
