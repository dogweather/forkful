---
title:    "Bash: 连接字符串"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么要学习 Bash 编程

Bash 是一种流行的脚本语言，可以帮助程序员轻松地自动化各种任务。其中一个常用的功能就是字符串拼接，通过将不同的字符串连接起来，可以创建出更复杂的文本内容。这在处理文本文件或者输出结果时非常有用。接下来我们将介绍如何使用 Bash 编程进行字符串拼接。

## 如何使用 Bash 进行字符串拼接

在 Bash 中，字符串被包含在引号中。例如，"Hello" 和 'World' 都是字符串。要将两个字符串拼接起来，可以使用 **$** 符号和花括号来引用变量，并使用 **+** 号来连接它们。下面是一个简单的例子：

```Bash
firstName="小明"
lastName="张"
fullName=${firstName}+${lastName}
echo $fullname
```

输出结果应为：小明张

如果要在字符串中添加空格，可以在连接符号 **+** 前后加上单引号或双引号。例如，如果我们想要在上面的例子中添加一个空格，可以写成下面这样：

```Bash
firstName="小明"
lastName="张"
fullName=${firstName}' + '${lastName}
echo $fullName
```

输出结果应为：小明 + 张

## 深入了解字符串拼接

除了上面的示例，Bash 还有许多其他方法来进行字符串拼接。例如，使用 **+=** 可以将新的字符串连接到现有变量的末尾。还可以使用 **printf** 命令来格式化字符串，然后再进行拼接。另外，使用 **read** 命令可以从用户输入中获取字符串，并与其他变量一起拼接。对于更复杂的字符串拼接，还可以使用 **sed** 或 **awk** 等其他工具来处理文本内容。

## 参考资料

- [Bash 字符串拼接方法](https://linuxize.com/post/bash-concatenate-strings/)
- [Bash 字符串操作指南](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Parameter-Expansion)
- [Linux Shell 简介](https://wangdoc.com/bash/guide.html)

## 参见
https://www.runoob.com/w3cnote/linux-shell-bash-string-concatenation.html
https://www.coder.work/article/3434136