---
title:    "Python: 字符串大写"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

为什么：首先，让我们来解释一下为什么有人需要在Python编程中用到字符串大写。通常，在编写程序时，我们需要对输入的数据进行格式化，使其在输出时更易读。在这种情况下，大写字符串可以使数据更加清晰和易读，从而提高代码的可读性。

如何使用：在Python中，要使一个字符串大写，我们可以使用内置的`upper()`方法。让我们来看一个例子：

```Python
string = "hello world"

print(string.upper())
```

输出结果是`HELLO WORLD`，可以看到字符串已经被转换为大写了。如果我们想要保持原始字符串的不变，我们也可以使用`capitalize()`方法来创建一个新的大写字符串。

深入探究：实际上，在Python中字符串是不可变的，这意味着一旦字符串被创建，它的值就不能被改变。因此，当我们使用`upper()`方法时，实际上是创建了一个新的大写字符串。另外值得注意的是，`upper()`方法只能将字符串中的小写字母转换为大写，如果字符串中已经有大写字母或其他字符，它们将保持不变。如果我们想要将字符串中的每个单词的首字母大写，我们可以使用`title()`方法。

另外一个在Python中大写字符串的常用场景是用于比较字符串。通过将两个字符串都转换为大写，我们可以忽略大小写的差异，从而更容易比较它们是否相等。

参考链接：
- [Python文档 - 字符串方法](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Python字符串教程](https://www.runoob.com/python/python-strings.html)