---
title:                "Haskell: 字符串的大写"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在Haskell编程中，我们经常需要处理字符串。有时候，我们需要将一个字符串中的所有字母变成大写。这可能是为了满足特定的格式要求，或者是为了方便处理数据。不管是什么原因，学会如何将字符串中的字母变成大写是非常有用的。

## 如何实现

在Haskell中，我们可以使用函数`toUpper`来将一个字符变成大写。我们也可以使用`map`函数来将一个字符串中的所有字符应用`toUpper`函数。下面是一个简单的例子：

```Haskell
strToUpper :: String -> String
strToUpper str = map toUpper str

main :: IO ()
main = do
  let str = "Hello World"
  let upperStr = strToUpper str
  putStrLn upperStr
```

运行上面的代码，会输出：

```
HELLO WORLD
```

## 深入探讨

在上面的例子中，我们使用了`map`函数来将函数`toUpper`应用到字符串的每一个字符上。但实际上，Haskell中还有更优雅的方法来实现这个功能。

我们可以使用列表推导式来直接将字符串中的每个字符转化成大写。下面是一个示例：

```Haskell
strToUpper :: String -> String
strtoupper str = [toUpper c | c <- str]

main :: IO ()
main = do
  let str = "Hello World"
  let upperStr = strToUpper str
  putStrLn upperStr
```

运行上面的代码，会获得和之前相同的输出。但是，使用列表推导式的方法更加简洁和易读。

## 参考链接

- [Haskell字符串教程](https://www.haskell.org/tutorial/strings.html)
- [Haskell标准文档 - toUpper函数](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html#v:toUpper)
- [Haskell标准文档 - map函数](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:map)
- [Haskell列表推导式教程](https://en.wikibooks.org/wiki/Haskell/List_comprehension)

## 参见

- [Haskell入门教程](https://www.haskell.org/documentation/)
- [Haskell String模块文档](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-String.html)