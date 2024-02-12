---
title:                "使用调试器"
aliases: - /zh/fish-shell/using-a-debugger.md
date:                  2024-01-26T03:49:06.562065-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用调试器"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何为调试及其原因？
使用调试器的全部目的是为了消灭错误——那些讨厌且耗时的代码错误。程序员之所以进行调试，是因为他们希望有效地找到并修复问题、理解代码流程，并更清晰地了解他们的代码实际上在做什么。

## 如何进行：
Fish没有像某些其他Shell那样内置的调试器，但你可以使用外部工具，比如用`gdb`来调试已编译的程序或者使用`fish -d`来在不同级别下运行fish并输出调试信息。我们来试试`fish -d`:

```fish
# 以调试级别2运行fish shell
fish -d2

# 在fish shell中，我们来测试一个可能含有错误的简单函数
function test_func
    set val 42
    echo "The value is $val"
    if test $val -eq 42
        echo "All is well."
    else
        echo "Something's fishy."
    end
end

# 调用这个函数并观察调试输出
test_func
```

你会看到在函数执行前后多了一些调试输出，帮助你锁定问题所在。

## 深入了解
在Unix-like环境中，历史上通常使用专门的工具进行调试，如用`gdb`调试C/C++或用`pdb`调试Python。在Fish中，你通常依赖于外部工具或内置特性，比如使用`functions -v`获得函数的详细输出，以及使用`set -x`来跟踪变量的变化。

有些人因为脚本调试特性如`set -x`而选择像Bash这样的其他Shell。然而，Fish以其对用户友好和互动性的关注而有其独到之处，这在很多情况下可以减少对硬核调试的需求。

就实现而言，调试脚本通常涉及以详细输出运行脚本并追踪变量在哪里被设置、取消设置或以意料之外的方式变化。有了Fish的彩色输出和用户友好的方法，你通常可以避免进行细致的调试——但当你卡住时，记住，详细性和清晰度是你最好的工具。

## 参见
当你在代码中陷得太深时，这些是一些值得信赖的生命线：

- Fish关于调试的文档：https://fishshell.com/docs/current/index.html#debugging
- GDB（GNU调试器）官方指南：https://www.gnu.org/software/gdb/documentation/
- Stack Overflow Fish标签 - 真实世界的调试案例：https://stackoverflow.com/questions/tagged/fish
- 高级Bash脚本指南 - 用来比较调试方法：https://tldp.org/LDP/abs/html/debugging.html
