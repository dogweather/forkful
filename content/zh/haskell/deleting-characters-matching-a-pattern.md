---
title:    "Haskell: 删除匹配模式的字符。"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，我们经常会遇到需要删除特定模式字符的情况。这可能是因为我们需要清理数据，或者我们需要在字符串中删除特定的字符。无论是什么原因，删除匹配模式的字符是一个非常有用的编程技巧，可以帮助我们更有效地处理数据和字符串。

## 如何做

在 Haskell 中，我们可以使用 `filter` 和 `notElem` 函数来删除匹配模式的字符。首先，我们创建一个函数 `deleteMatching` 来接收一个模式和一个字符串作为参数。然后，我们使用 `filter` 函数来筛选出不满足模式的字符，并使用 `notElem` 函数来判断字符是否不等于模式。最后，我们返回经过筛选的字符列表。

```Haskell
deleteMatching :: Eq a => a -> [a] -> [a]
deleteMatching pattern string = filter (`notElem` [pattern]) string
```

让我们来尝试一下使用这个函数：

```Haskell
deleteMatching 'a' "apple"
```

输出结果为：

```
"pple"
```

我们也可以将这个函数用于字符串列表：

```Haskell
deleteMatching 'o' ["hello", "world", "how", "are", "you"]
```

输出结果为：

```
["hell", "wrld", "hw", "are", "yu"]
```

## 深入探讨

尽管我们可以使用 `filter` 和 `notElem` 函数来删除匹配模式的字符，但有时候我们可能需要更复杂的模式匹配方法。在这种情况下，我们可以使用正则表达式库 `regex-tdfa` 来帮助我们。

首先，我们需要导入 `regex-tdfa` 库：

```Haskell
import Text.Regex.TDFA
```

然后，我们使用 `subRegex` 函数来替换正则表达式匹配的字符。让我们尝试将字符串中所有的数字替换为空字符：

```Haskell
subRegex (mkRegex "[0-9]") "abc123xyz" ""
```

输出结果为：

```
"abcxyz"
```

## 参考阅读

- [Haskell Wiki: filter](https://wiki.haskell.org/Filter)
- [Haskell Wiki: notElem](https://wiki.haskell.org/NotElem)
- [Hackage: regex-tdfa](https://hackage.haskell.org/package/regex-tdfa)