---
title:                "编写测试"
aliases:
- zh/fish-shell/writing-tests.md
date:                  2024-02-03T19:30:34.274664-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写测试"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Fish Shell 中编写测试涉及到创建脚本，这些脚本会自动运行你的代码以验证其行为是否符合预期结果。这种做法至关重要，因为它确保你的 Shell 脚本按预期工作，能够及早发现错误并简化维护工作。

## 如何操作：

Fish 没有像一些其他编程环境那样内置的测试框架。然而，你可以编写使用断言来检查函数行为的简单测试脚本。此外，你可以利用第三方工具如 `fishtape` 来获得更全面的测试套件。

### 示例 1：基本测试脚本

我们从一个在 Fish 中计算两个数之和的基本函数开始：

```fish
function add --description 'Add two numbers'
    set -l sum (math $argv[1] + $argv[2])
    echo $sum
end
```

你可以为这个函数编写一个基本测试脚本，如下所示：

```fish
function test_add
    set -l result (add 3 4)
    if test $result -eq 7
        echo "test_add passed"
    else
        echo "test_add failed"
    end
end

test_add
```

运行这个脚本将输出：

```
test_add passed
```

### 示例 2：使用 Fishtape

为了得到一个更健壮的测试解决方案，你可以使用 `fishtape`，这是一个为 Fish 生成 TAP 的测试运行器。

首先，如果你还没有安装 `fishtape`，则安装它：

```fish
fisher install jorgebucaran/fishtape
```

接下来，为你的 `add` 函数创建一个测试文件，例如 `add_test.fish`：

```fish
test "Adding 3 and 4 yields 7"
    set result (add 3 4)
    echo "$result" | fishtape
end
```

使用以下命令运行测试：

```fish
fishtape add_test.fish
```

样本输出可能如下所示：

```
TAP version 13
# Adding 3 and 4 yields 7
ok 1 - test_add passed
```

这告诉你测试成功通过了。`fishtape` 允许你构建更详细的测试并提供有用的输出，便于调试并为你的 Fish 脚本提供全面的测试覆盖。
