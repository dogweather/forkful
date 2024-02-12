---
title:                "将代码组织成函数"
aliases:
- /zh/fish-shell/organizing-code-into-functions/
date:                  2024-01-28T23:01:28.563755-07:00
model:                 gpt-4-0125-preview
simple_title:         "将代码组织成函数"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, dogweather, reviewed and added links
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?
将代码组织成函数是指将脚本的一部分打包起来执行特定任务。我们这么做是因为它使代码更易于阅读、测试和重用 —— 没有人想要在一堆代码意大利面中挣扎。

## 如何操作:
在Fish中，你可以使用`function`关键字编写一个函数，给它一个名称，并以`end`结束。这里有一个简单的例子：

```fish
function hello
    echo "Hello, World!"
end

hello
```

输出：
```
Hello, World!
```

现在，让我们使它向用户问好：

```fish
function greet
    set user (whoami)
    echo "Hey there, $user!"
end

greet
```

输出：
```
Hey there, your_username!
```

要想在多个会话中保存它，使用`funcsave greet`。

## 深入探究
Fish Shell的函数就像迷你脚本——你几乎可以在里面放任何东西。从历史上看，在shell脚本中函数的概念已经节省了无数小时的重复性键入和调试。与Python这样的编程语言不同，Shell函数更多的是关于便利性而非结构。

一些shell，比如Bash，使用`function`或者直接使用大括号。Fish坚持使用`function ... end`——清晰且易读。在Fish函数内部，你可以享受所有的功能：参数、使用`set -l`设置的局部变量，甚至可以在另一个函数内定义一个函数。

你不需要一个`return`值，因为Fish在这方面不太重视；你的函数的输出就是它的返回。如果你想要将函数持久化，以供未来的会话使用，记住`funcsave`。

## 另请参阅

- Fish教程中的函数部分: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### 函数命令

- [function](https://fishshell.com/docs/current/cmds/function.html) — 创建一个函数
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — 打印或删除函数
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — 将函数的定义保存到用户的自动加载目录中
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — 交互式编辑函数
