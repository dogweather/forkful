---
title:    "Bash: 寻找字符串的长度"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## 为什么
为什么有必要去寻找字符串的长度？当我们在处理文本、编写命令行程序或者进行数据分析时，经常会遇到需要知道字符串长度的情况。因此，掌握如何找到字符串的长度是很有用的技能，能够帮助我们更高效地完成任务。

## 如何
在Bash中，使用内置命令`echo`和重定向符号`>`可以轻松地获取字符串的长度。例如，我们要找到字符串`Hello World`的长度，可以输入以下命令，并在命令行界面按下回车键：
```
Bash
echo "Hello World" > len     # 使用重定向符将字符串输出到名为len的文件中
wc -c len                    # 使用内置命令wc来计算len文件的字符数，即字符串长度
```
命令行界面会返回`12`，表示字符串`Hello World`的长度为12个字符。

同样，在Shell脚本中，我们也可以使用`echo`和重定向符号来获取字符串的长度。下面是一个例子：
```
Bash
#!/bin/bash
str="Hello World"
len=$(echo -n $str | wc -c)   # 使用echo命令的选项-n来取消输出行末的换行符，避免计算长度时将换行符也计入
echo "字符串$str的长度为$len个字符"
```
运行脚本后，会输出`字符串Hello World的长度为12个字符`。

## 深入了解
在Bash中，`echo`命令会将给定的参数输出到标准输出，而使用重定向符号`>`则可以将输出重定向到一个文件中。然后，在使用内置命令`wc`时，选项-c表示统计字符数。

另外，还有一些其他的方法可以获取字符串的长度，比如使用`expr`命令、使用`\"`字符来转义特殊字符等。感兴趣的读者可以通过阅读相关文档来进一步学习。

## 参考资料
- [Linux命令-wc](https://www.runoob.com/linux/linux-comm-wc.html)
- [Bash Shell中取得字符串长度的方法](https://www.cnblogs.com/belief3309/archive/2013/10/14/3367053.html)

## 另请参阅
- [Bash Shell教程](https://www.runoob.com/linux/linux-shell.html)
- [Bash Shell官方文档](https://www.gnu.org/software/bash/manual/bash.html)