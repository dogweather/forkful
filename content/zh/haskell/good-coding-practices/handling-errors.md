---
date: 2024-01-26 00:53:20.593032-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\u5904\u7406\u9519\u8BEF\u662F\u5173\u4E8E\u7BA1\
  \u7406\u610F\u6599\u4E4B\u5916\u7684\u4E8B\u60C5\u2014\u2014\u53EF\u80FD\u51FA\u9519\
  \u7684\u4E8B\u60C5\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u786E\
  \u4FDD\u4ED6\u4EEC\u7684\u7A0B\u5E8F\u80FD\u591F\u4F18\u96C5\u5730\u5E94\u5BF9\u8FD9\
  \u4E9B\u60C5\u51B5\uFF0C\u800C\u4E0D\u662F\u5D29\u6E83\u6216\u4EA7\u751F\u9519\u8BEF\
  \u7684\u7ED3\u679C\u3002"
lastmod: '2024-03-13T22:44:47.824456-06:00'
model: gpt-4-1106-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\u5904\u7406\u9519\u8BEF\u662F\u5173\u4E8E\u7BA1\
  \u7406\u610F\u6599\u4E4B\u5916\u7684\u4E8B\u60C5\u2014\u2014\u53EF\u80FD\u51FA\u9519\
  \u7684\u4E8B\u60C5\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u786E\
  \u4FDD\u4ED6\u4EEC\u7684\u7A0B\u5E8F\u80FD\u591F\u4F18\u96C5\u5730\u5E94\u5BF9\u8FD9\
  \u4E9B\u60C5\u51B5\uFF0C\u800C\u4E0D\u662F\u5D29\u6E83\u6216\u4EA7\u751F\u9519\u8BEF\
  \u7684\u7ED3\u679C\u3002"
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

## 什么 & 为什么?
在编程中处理错误是关于管理意料之外的事情——可能出错的事情。程序员这样做是为了确保他们的程序能够优雅地应对这些情况，而不是崩溃或产生错误的结果。

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
