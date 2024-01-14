---
title:    "Haskell: 将字符串转换为小写"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 为什么转换字符串为小写

转换字符串为小写是一种常见的操作，它可以让我们处理字符串时更加方便和灵活。比如，我们可以将用户输入的字符串统一转换成小写，这样就不会因为大小写的不同而造成误匹配。

## 如何实现

在Haskell中，我们可以使用 `map` 函数来实现字符串的小写转换。 `map` 函数接受一个函数和一个列表作为参数，它将会将列表中的每个元素都应用到函数上，并返回一个新的列表。

``` Haskell
import Data.Char (toLower)

-- 定义一个函数来转换字符串为小写
toLowerString :: String -> String
toLowerString = map toLower
```

``` Haskell
main = do
    let str = "HeLlo, WoRld!"
    putStrLn $ "原始字符串：" ++ str
    putStrLn $ "转换为小写后：" ++ toLowerString str
```

``` bash
原始字符串：HeLlo, WoRld!
转换为小写后：hello, world!
```

## 深入探究

在上面的例子中，我们使用了 `Data.Char` 模块中的 `toLower` 函数来转换字符为小写。除了 `toLower` 函数之外，还有其他一些函数也可以达到同样的效果，比如 `toLowerAscii`、`toLowerUnicode` 等，它们之间的区别在于是否支持Unicode字符。

此外，我们也可以通过组合函数来实现复杂的字符串操作。下面的例子中，我们使用 `words` 函数来将字符串按照空格拆分为单词，然后使用 `toLowerString` 函数来转换每个单词为小写，最后使用 `unwords` 函数将单词拼接回去。

``` Haskell
main = do
    let str = "Hello, World!"
    putStrLn $ "原始字符串：" ++ str
    putStrLn $ "单词转换为小写后：" ++ (unwords . map toLowerString . words) str
```

``` bash
原始字符串：Hello, World!
单词转换为小写后：hello, world!
```

# 参考链接

- [Haskell Data.Char模块文档](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Haskell Map函数文档](https://hackage.haskell.org/package/base/docs/Prelude.html#v:map)
- [Haskell String类型文档](https://hackage.haskell.org/package/base/docs/Prelude.html#t:String)
- [Haskell 初学者指南（中文）](http://learnyouahaskell.zbzzbq.com/)