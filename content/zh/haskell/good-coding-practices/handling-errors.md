---
date: 2024-01-26 00:53:20.593032-07:00
description: "\u5982\u4F55\u64CD\u4F5C: Haskell\u901A\u8FC7\u50CF `Maybe` \u548C `Either`\
  \ \u8FD9\u6837\u7684\u7C7B\u578B\u6765\u5065\u58EE\u5730\u5904\u7406\u9519\u8BEF\
  \u3002\u8FD9\u91CC\u662F\u4E00\u4E2A\u5FEB\u901F\u7684\u56DE\u987E\uFF1A."
lastmod: '2024-03-13T22:44:47.824456-06:00'
model: gpt-4-1106-preview
summary: "Haskell\u901A\u8FC7\u50CF `Maybe` \u548C `Either` \u8FD9\u6837\u7684\u7C7B\
  \u578B\u6765\u5065\u58EE\u5730\u5904\u7406\u9519\u8BEF\u3002\u8FD9\u91CC\u662F\u4E00\
  \u4E2A\u5FEB\u901F\u7684\u56DE\u987E\uFF1A."
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

## 如何操作:
Haskell通过像 `Maybe` 和 `Either` 这样的类型来健壮地处理错误。这里是一个快速的回顾：

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- 除以零是不行的，所以我们返回 Nothing。
safeDivide x y = Just (x `div` y)  -- 否则，一切都好，返回结果在一个 Just 中。

-- 让我们看看它的行动:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

对于更复杂的错误处理，`Either` 发挥作用：

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Divide by zero error."  -- 这次，错误带有一条信息。
safeDivideEither x y = Right (x `div` y)

-- 使用中:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Divide by zero error."
```

## 深入了解
在 Haskell 的世界里，错误处理有着悠久的历史。在过去，错误可能会导致你的整个程序崩溃——一点也不有趣。Haskell 的类型系统提供了让这种情况不太可能发生的方法。我们有 `Maybe` 和 `Either`，但还有其他像 `Exceptions` 和 `IO` 的类型用于不同的情境。

`Maybe` 很简单：如果一切都好，你得到 `Just` 某物；如果不好，你得到 `Nothing`。`Either` 更进一步，允许你返回一个错误信息 (`Left`) 或一个成功的结果 (`Right`)。

它们都是纯的，意味着它们不会影响外部世界 — 在 Haskell 中这是个大事。我们避免了一些其他语言中普遍存在的未检查异常的陷阱。

对于那些对 `Maybe` 和 `Either` 不满足的人，像 `Control.Exception` 这样的库通过异常提供了更传统的、命令式风格的错误处理。但是过度使用它们可能会使事情变复杂，因此社区通常坚持使用类型。

## 另请参阅
深入了解：

- Haskell 自己的文档：[Haskell](https://haskell.org/documentation)
- 非常适合初学者：["Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/)
