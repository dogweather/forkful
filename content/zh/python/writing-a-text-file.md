---
title:    "Python: 撰寫文本文件"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 为什么：

在Python编程中，写入文本文件是一个非常有用的技能。它可以让我们保存数据和信息，并可以随时读取和处理它们。无论是做一个数据分析项目还是创建一个网络爬虫，写入文本文件都是十分必要的步骤。同时，它也是一个很好的练习，可以帮助我们更好地了解Python的IO操作和字符串处理。

## 如何：

写入文本文件非常简单，我们只需要使用内置的`open()`函数，向其传入文件名和打开模式。最常用的打开模式是`w`，它表示写入模式。然后，我们可以使用`write()`函数来写入我们想要保存的信息。下面是一个简单的示例：

```python
file_name = "my_file.txt"

# 使用"with"语句打开文件，可以在文件操作结束后自动关闭文件
with open(file_name, "w") as file:
    file.write("这是我写入文本文件的信息！") # 写入信息
```

这样，我们就创建了一个名为`my_file.txt`的文本文件，并将字符串`这是我写入文本文件的信息！`写入其中。你可以尝试运行以上代码，并查看`my_file.txt`文件中的内容。

## 深入探讨：

除了上面提到的`w`模式外，我们还可以使用`a`模式来进行追加写入，即在已有文件的末尾添加新信息而不覆盖原有信息。另外，`r`模式用于读取文件内容，而`r+`模式允许我们既读取又写入文件。除了这些基本的模式之外，我们还可以使用`with open() as file`语法中的`file`对象的其他方法来读取、写入、删除等操作文件。

# 参考链接：

- [Python文本文件操作教程](https://www.runoob.com/python/file-methods.html)
- [用Python爬取网站内容并保存为文本文件](https://zhuanlan.zhihu.com/p/34549217)
- [如何用Python进行数据分析](https://www.zhihu.com/question/39022858/answer/823754227)
- [Python字符串处理教程](https://www.runoob.com/python/python-strings.html)

# 参见：

- [Python字符串编码指南](https://zhuanlan.zhihu.com/p/81605102)
- [关于Markdown文件的基本操作](https://www.jianshu.com/p/191d1e21f7ed)
- [更多Python编程技巧和建议](https://www.zhihu.com/question/19920440/answer/103447300)