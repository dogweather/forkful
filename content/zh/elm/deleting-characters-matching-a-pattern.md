---
title:    "Elm: 删除匹配模式的字符"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 为什么

为什么会想要删除匹配某种模式的字符？也许你在处理大量数据时想要清理不需要的字符，或者你想要给用户提供更好的输入验证。

## 如何进行

```Elm
-- 在字符串中删除所有匹配模式的字符
deletePattern : String -> String -> String
deletePattern pattern string =
  String.filter (\char -> not (String.contains pattern (String.fromChar char))) string
```

输入： `"Hello, World!"`

输出： `"Hello!"`

## 深入讨论

删除匹配某种模式的字符可能看起来很简单，但是在细节上还是有一些需要注意的地方。比如，删除的模式可以是正则表达式，而不仅仅是一个简单的字符串，这在编写通用代码时就有用。另外，虽然上面的函数是一个纯函数，但是如果你希望能够在原始字符串上进行删除而不是返回一个新的字符串，你可以使用 `String.indexes` 函数来获取匹配的字符在原始字符串中的索引，然后将这些索引传递给 `String.Extra.splice` 函数来改变原始字符串。总的来说，这个看似简单的问题有着更多的细节需要考虑。

## 查看相关资料

- [Elm语言官方文档](https://guide.elm-lang.org/)
- [Elm社区论坛](https://discourse.elm-lang.org/)
- [正则表达式入门教程](https://www.runoob.com/regexp/regexp-tutorial.html)