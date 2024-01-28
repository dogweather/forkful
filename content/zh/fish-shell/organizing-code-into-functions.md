---
title:                "将代码组织成函数"
date:                  2024-01-26T01:10:35.386804-07:00
model:                 gpt-4-1106-preview
simple_title:         "将代码组织成函数"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 何为函数 & 为何要用函数？
将代码组织成函数是指将脚本的各个部分打包成完成特定任务的单元。我们这样做是因为它使代码更易于阅读、测试和重用 —— 没有人想泥足深陷于代码意大利面的泥潭中。

## 如何操作：
在Fish中，你可以使用 `function` 关键字编写一个函数，给它命名，然后用 `end` 结束。这里有一个简单的示例：

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

现在，让我们编写一个打招呼的函数：

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

若要在会话间保存它，使用 `funcsave greet`。

## 深入了解
Fish Shell函数就像迷你脚本 —— 你几乎可以在里面放入任何东西。历史上，在shell脚本中函数的概念节约了无数小时的重复性打字和调试工作。不像Python等编程语言，Shell函数更多的是为了便利而不是结构。

一些Shell，如Bash，使用 `function` 或直接使用大括号。而Fish坚持使用 `function ... end` —— 清晰且易读。在Fish函数内部，你可以使用所有的功能：参数、使用 `set -l` 设定局部变量，甚至可以在一个函数内部定义另一个函数。

你不需要 `return` 值，因为Fish并不重视这一点；你的函数的输出就是它的返回。如果你想要持久的函数，以便未来的会话中可用，记得使用 `funcsave`。

## 另见
- Fish关于函数的教程：https://fishshell.com/docs/current/tutorial.html#tut_functions
- Fish中 `function` 的文档：https://fishshell.com/docs/current/cmds/function.html
- 在fish中编写函数的详尽指南：https://fishshell.com/docs/current/index.html#syntax-function
