---
title:                "将代码组织成函数"
aliases:
- /zh/haskell/organizing-code-into-functions/
date:                  2024-01-26T01:10:30.687313-07:00
model:                 gpt-4-1106-preview
simple_title:         "将代码组织成函数"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 何为之 & 为何之?
在 Haskell 中组织代码进入函数意味着将你的代码分解成可复用的、有名称的代码块。为什么这么做？它能使你的代码遵循"不要重复自己"(Don't Repeat Yourself，即 DRY)原则，使代码可读性更强，且更易于调试。

## 如何操作:
下面是你在 Haskell 中编写和使用函数的方式：

```Haskell
-- 定义一个简单的函数来添加两个数字
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- 使用该函数
main = print (addNumbers 3 5)
```

输出:
```
8
```

你也可以创建高阶函数：

```Haskell
-- 接受一个函数并将其应用于某物两次
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- 使用 applyTwice 与匿名函数
main = print (applyTwice (*2) 5)
```

输出:
```
20
```

## 深入探讨
Haskell 是一种纯函数式编程语言，它将函数视为一等公民。在历史上，这根植于作为计算基础框架的 lambda 演算。不同于命令式语言那样，函数是一序列的指令，在 Haskell 中函数是描述数据之间关系的表达式。

除了编写原始函数以便复用，还有其他选择。考虑使用类型类来进行多态性，或者使用模块来分组相关函数。Haskell 的惰性求值也影响了函数的实现——函数不会被评估，直到需要它们的结果，这可能会影响性能考虑。

## 另请参阅
- 官方 Haskell 文档：https://www.haskell.org/documentation/
- 由 Miran Lipovača 编写、适合初学者的书籍《学你一哈斯克尔，以求大好》(Learn You a Haskell for Great Good!)：http://learnyouahaskell.com/
- Bryan O'Sullivan、Don Stewart 和 John Goerzen 编写的《实战 Haskell》(Real World Haskell)：http://book.realworldhaskell.org/
