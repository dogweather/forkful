---
title:    "Go: 寻找字符串的长度"
keywords: ["Go"]
---

{{< edit_this_page >}}

# 为什么

有时候，在编程中我们需要知道一个字符串的长度。这有助于我们在处理字符串时更清晰地了解它的大小和结构。在Go语言中，找到字符串的长度非常简单，下面让我们一起来学习如何做到这一点吧！

## 如何

```Go
func main() {
    // 定义一个字符串
    str := "你好，世界！"
    // 使用len方法获取字符串的长度
    length := len(str)
    // 打印输出
    fmt.Println("字符串的长度为：", length) 
}

// 输出结果：
// 字符串的长度为： 8
```

上面的代码中，我们首先定义了一个字符串变量 `str`，然后使用 `len` 方法获取字符串的长度，并将结果赋值给 `length` 变量。最后，我们打印输出字符串的长度，运行后会输出 `8`，因为“你好，世界！”一共有8个字符。

除了使用 `len` 方法，Go语言还提供了一个 `RuneCountInString` 方法来获取字符串中的Unicode字符数量。代码如下：

```Go
func main() {
    // 定义一个字符串
    str := "你好，世界！"
    // 使用RuneCountInString方法获取字符串中的Unicode字符数量
    length := utf8.RuneCountInString(str)
    // 打印输出
    fmt.Println("字符串的长度为：", length) 
}

// 输出结果：
// 字符串的长度为： 5
```

这里的输出结果为 `5` 是因为“你好，世界！”中有5个Unicode字符，即“你”、“好”、“，”、“世”、“界”。

## 深入探讨

那么，为什么要使用 `RuneCountInString` 方法呢？因为在Go语言中，一个字符并不一定就是一个字节，有些字符可能占用多个字节，如中文字符。而 `len` 方法只能统计字节的数量，无法准确地统计字符的数量。所以，如果需要统计Unicode字符数量，最好使用 `RuneCountInString` 方法。

此外，还有一个 `strings` 包提供了一些实用的方法来操作字符串，如 `strings.Count` 方法可以统计字符串中指定子串出现的次数，`strings.Contains` 方法可以判断一个字符串是否包含另一个子字符串，`strings.HasPrefix` 和 `strings.HasSuffix` 方法可以判断一个字符串是否以指定前缀或后缀开头或结尾。更多用法，可以查阅官方文档。

# 参考链接

- [Go语言文档-字符串类型](https://go-zh.org/doc/code.html#string)
- [Go语言文档-strings包](https://go-zh.org/pkg/strings/)
- [Go语言文档-utf8包](https://go-zh.org/pkg/utf8/)