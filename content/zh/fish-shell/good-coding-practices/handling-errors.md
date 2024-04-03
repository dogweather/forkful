---
date: 2024-01-26 00:52:31.641452-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728Fish\u4E2D\u6355\u6349\u9519\
  \u8BEF\uFF0C\u8BF7\u501A\u9760`status`\u547D\u4EE4\u548C\u6761\u4EF6\u5224\u65AD\
  \u3002\u6BD4\u5982\u8BF4`ping`\u5931\u8D25\u4E86\uFF1B\u4E0B\u9762\u662F\u5982\u4F55\
  \u68C0\u6D4B\u7684\uFF1A."
lastmod: '2024-03-13T22:44:48.274010-06:00'
model: gpt-4-1106-preview
summary: "\u8981\u5728Fish\u4E2D\u6355\u6349\u9519\u8BEF\uFF0C\u8BF7\u501A\u9760`status`\u547D\
  \u4EE4\u548C\u6761\u4EF6\u5224\u65AD\u3002\u6BD4\u5982\u8BF4`ping`\u5931\u8D25\u4E86\
  \uFF1B\u4E0B\u9762\u662F\u5982\u4F55\u68C0\u6D4B\u7684\uFF1A."
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

## 如何操作：
要在Fish中捕捉错误，请倚靠`status`命令和条件判断。比如说`ping`失败了；下面是如何检测的：

```fish
ping -c 1 example.com
if not status is-success
    echo "ping时出现了些问题。"
end
```

若`ping`失败时的样例输出：

```
ping时出现了些问题。
```

要处理特定错误代码，请使用`status --is`：

```fish
false
if status --is 1
    echo "捕捉到错误代码1。"
end
```

样例输出：
```
捕捉到错误代码1。
```

为了更稳健的处理方法，考虑使用一个函数：

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping失败，状态码为$status"
        return 1
    end
end

try_ping
```

## 深入了解
在Fish中的错误处理并不像你可能从高级语言中了解的`try/catch`范式。相反，你拥有由`status`命令提供的直截了当的退出状态。

历史上，在类Unix系统中，退出状态为`0`表示成功，而任何非零值都表示错误，通常反映了不同的失败原因。大部分命令行工具以及Fish本身都采用了这一惯例。

在Fish中，替代`status`检查的方法包括在其他Shell中通过`trap`进行信号处理，但Fish更倾向于更显式的状态检查，因为这样更干净，且不太容易产生副作用。

在实现层面上，Fish中的错误处理虽简单但强大，这主要归功于其非阻塞性质和强调清晰语法，如示例所示。错误代码与函数很好地融合在一起，允许模块化和可读的错误管理。

## 参见
- Fish文档中关于条件判断的部分：https://fishshell.com/docs/current/language.html#conditionals
- Fish关于错误处理的教程：https://fishshell.com/docs/current/tutorial.html#error-handling
