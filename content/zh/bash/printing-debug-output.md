---
title:                "打印调试输出"
html_title:           "Bash: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么 & 为什么

打印调试输出是一种程序员经常做的事情，它可以帮助我们在程序执行的过程中发现错误。通过输出程序中特定的变量、值或者语句，我们可以更容易地排查问题，并使我们的代码更加可靠。

## 如何

### 用变量打印调试输出

```Bash
name="John"
echo "My name is $name."
```

输出：

```
My name is John.
```

### 打印整个数组

```Bash
fruit=("apple" "orange" "banana")
echo "I like ${fruit[*]}s."
```

输出：

```
I like apples, oranges, and bananas.
```

### 使用条件语句打印调试输出

```Bash
age=25
if [ "$age" -ge 18 ]; then
  echo "You are an adult."
else
  echo "You are a minor."
fi
```

输出：

```
You are an adult.
```

## 深入了解

### 历史背景

打印调试输出是一种早期的程序员调试技术。在没有现代的调试工具时，程序员通过手动输出特定的信息来发现错误，这也促进了代码的可读性。

### 替代方法

除了打印调试输出，程序员还可以使用调试工具来检查程序的执行过程。这些工具可以提供更多详细的信息，并且更加方便和高效。

### 实现细节

在Bash中，我们可以使用echo命令来打印调试输出。它可以接受多个参数，或者通过使用字符串格式化来打印变量的值。

## 参考

- [Bash 脚本输出调试信息的方法](https://blog.csdn.net/u010571489/article/details/79268657)
- [Bash 调试技巧](https://everyday.codes/bash-debugging-tips/)
- [Bash 调试工具介绍](https://www.shellcheck.net/)