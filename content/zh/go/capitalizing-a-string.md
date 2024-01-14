---
title:    "Go: 字符串大写化"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

为什么：每个程序员都需要学习字符串大写的操作。这可以让我们更高效地处理文本数据，并且在某些情况下，格式化的文本可能要求字符串大写。

如何做：首先，我们需要导入 "strings"包。然后，通过使用 strings.ToUpper函数将字符串转换为大写。以下是一个简单的例子：

```Go
import "strings"
str := "hello world"
upperStr := strings.ToUpper(str)
fmt.Println(upperStr)
```
输出：HELLO WORLD

深入了解：字符串大写操作其实是通过ASCII码实现的。在ASCII码表中，小写字母的值比大写字母大32。因此，通过将字符串中的每个字符的值加上32即可实现大写操作。值得注意的是，这种方法只适用于英文字母，对于其他字符可能不适用。

此外，还有一个 strings.Title函数可以实现字符串中每个单词的首字母大写。例如：

```Go
str := "hello world"
titleStr := strings.Title(str)
fmt.Println(titleStr)
```
输出：Hello World

另外，对于中文字符的大写操作，可以使用unicode.ToLower和unicode.ToUpper函数。

详细了解字符串大写操作的原理和方式可以帮助我们更好地理解和运用这个功能。

参考资料：

- [Go语言中文网 - 字符串](https://studygolang.com/pkgdoc)
- [Golang中国 - 字符串的各种操作](https://www.golangtc.com/t/68d821ae320b724c3207002a)



## 另请参阅：
- [Go语言字符串操作入门指南](https://www.jianshu.com/p/0ffb0f6cb519)
- [Golang中文网 - 字符串操作函数](https://studygolang.com/pkgdoc)