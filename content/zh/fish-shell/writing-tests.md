---
title:                "编写测试代码"
date:                  2024-01-19
simple_title:         "编写测试代码"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
编写测试是创建脚本来自动检查代码功能的过程。程序员这样做是为了保障代码质量，发现并修正错误以及防止未来的回归。

## 如何：
```Fish Shell
function test_add
    set result (math 2+2)
    if test $result = 4
        echo "加法测试通过"
    else 
        echo "加法测试失败: 预期 4, 获得 $result"
    end
end

test_add
```
输出样例：
```
加法测试通过
```

## 深入探索
Fish Shell 最早在 2005 年发布，目的是提供一个更智能、更用户友好的命令行环境。尽管 Fish 不自带传统意义上的测试框架，但你可以通过内置函数和条件语句来实现基本测试。对于更复杂的测试需求，你可以使用第三方工具，比如 Fishtape 或 Fisher。

## 相关链接
- Fish Shell 官网: [https://fishshell.com](https://fishshell.com)
- Fishtape, 一个 Fish Shell 测试框架: [https://github.com/jorgebucaran/fishtape](https://github.com/jorgebucaran/fishtape)
- Fisher, 一个 Fish Shell 插件管理器: [https://github.com/jorgebucaran/fisher](https://github.com/jorgebucaran/fisher)
