---
title:    "Haskell: 字串大写"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 为什么：字符大写的意义

在编程过程中，我们经常需要处理字符串（String）类型的数据。有时候，我们希望将字符串中的某些字母改为大写，比如在用户输入时进行格式规范化，或者在输出时让文字更醒目。在Haskell中，我们可以使用内置函数`toUpper`来实现这一功能。

## 如何操作

首先导入`Data.Char`模块，该模块中包含了`toUpper`函数。然后，我们可以使用`toUpper`来将字符串中的所有字母改为大写，如下所示：

```Haskell
import Data.Char

capitalize :: String -> String
capitalize str = map toUpper str

main = do
  let str = "hello world"
  putStrLn $ capitalize str
  -- 输出为：HELLO WORLD
```

## 深入探讨

在以上示例中，我们使用了`map`函数来将`toUpper`应用到字符串的每一个字符上，并返回一个新的字符串。这就是Haskell函数式编程的优势，我们可以通过组合现有的函数来实现复杂的功能，而不需要手动操作每一个字符。

另外，需要注意的是，`toUpper`函数只能将英文字母转换为大写，对于其他字符（比如标点符号和中文）并不会有任何改变。如果需要将所有字符都转换为大写，可以使用`Data.Text`模块中的`toUpper`函数。

# 参考链接

- [Haskell中的String数据类型](https://wiki.haskell.org/Strings)
- [Hoogle中的toUpper函数文档](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html#v:toUpper)
- [更多关于Haskell中字符串处理的函数](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#g:20)