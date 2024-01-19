---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么和为什么？

命令行参数读取是一种在启动程序时获取用户指定信息的方式。程序员这样做是因为这样可以在执行程序的时候，让用户提供自定义的输入，以改变程序行为。

## 如何做到：

在Lua中，我们可以使用全局变量arg，这是一个包含命令行参数的表。以下是一个简单的例子：

```Lua
for i = 0, #arg do
    print(arg[i])
end
```

假设我们将该程序命名为`args.lua`，并将其通过命令行进行调用：“lua args.lua one two”。输出将会是：

```Lua
lua
args.lua
one
two
```

## 深入了解：

命令行参数的使用可以追溯到早期的Unix系统，这是一种灵活的方法，允许用户在程序运行时提供特定选项或输入。

当然还有其他方式来获取用户输入，例如stdin，界面交互等，但命令行参数的使用在诸多情况下更为直接便利。

Lua通过存储在arg表中的命令行参数来实现，在Lua启动时，Lua会自动将所有命令行参数填充到arg表中，可以通过arg的索引来访问这些参数。值得注意的是，arg[-1]是解释器，而arg[0]是脚本名。

## 参见：

以下链接包含与命令行参数相关的更多深入细节和示例：

[Beispielcode auf Github](https://github.com/lua/lua/blob/master/testes/args.lua)

[Lua-用户手册](https://www.lua.org/manual/5.4/manual.html#pdf-arg)

在开始您的编程旅程时，理解并掌握这些概念是非常有价值的。祝您编程愉快！