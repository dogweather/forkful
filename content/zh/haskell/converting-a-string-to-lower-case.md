---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么和为什么？

把字符串转为小写，意味着将所有大写字母转为相应的小写字母。程序员这么做主要为了数据规范化和比较，将用户输入的大写和小写字符视为相同。

## 如何操作:

让我们看怎么用Haskell来转换字符串，让其中的所有字符都变为小写。Haskell提供了一个内置函数 `toLower` 来做这个转换。

```Haskell
import Data.Char (toLower)

lowerCaseStr :: String -> String
lowerCaseStr = map toLower

main = print (lowerCaseStr "Hello, Haskell!")
```
运行这段代码，输出结果应该是 "hello, haskell!"。

## 深入了解

让我们深入了解一下这个问题。在软件发展的早期，人们为了方便，开始对大小写视而不见。这就是我们为什么要转换字符串的原因。至于Haskell使用的`toLower`，其实它来自Unicode规范。换言之，它可以处理任何Unicode字符，转换的结果和我们在其他编程语言里看到的类似。

有时，我们可能会需要其他方式来转小写。比如在Haskell中，`Data.Text.Lazy`和`Data.Text`两个库就提供了类似的方法。但是你都要注意效率问题。

```Haskell
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

lowerCaseText :: T.Text -> T.Text
lowerCaseText = T.toLower

lowerCaseLazyText :: TL.Text -> TL.Text
lowerCaseLazyText = TL.toLower
```

## 另请参阅

了解更多关于`toLower`和字符转换的详情，你可以在Haskell官方文档中查看：
- [Haskell Data.Char](http://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Haskell Data.Text](https://hackage.haskell.org/package/text)
- [Haskell Data.Text.Lazy](http://hackage.haskell.org/package/text/docs/Data-Text-Lazy.html)

好了，这就是我们今天关于Haskell如何将字符串转为小写的全部内容。记住，这只是许多可能故事的其中之一。