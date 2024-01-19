---
title:                "使用正则表达式"
html_title:           "Arduino: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

---

## 什么 & 为什么？

正则表达式（Regular expressions）是用来检查和操作字符串的强大工具。程序员使用正则表达式来节省时间，提高编程效率。

---

## 操作说明：

用Bash用正则表达式的代码例子如下：

```Bash
#!/bin/bash
text="Welcome to AI world."
pattern="AI"

if [[ $text =~ $pattern ]]; then
   echo "Pattern found!"
fi
```

运行这段代码，输出就是 "Pattern found!"。如果你修改文本里的"AI"，输出就会变。

---

## 深入解析：

### 历史：正则表达式最早在上世纪60年代由Ken Thompson所创。起初是作为Unix操作系统的一部分，现在已经成为几乎所有操作系统和编程语言不可或缺的一部分。

### 另类：正则表达式是处理字符串非常强大的工具，尽管有些编程语言提供了相似的功能，比如Python的字符串方法，但是它们的功能还是无法和正则表达式相比。

### 实现细节：当你在Bash脚本中使用正则表达式时，你实际上是在调用GNU的正则表达式库。这个库包含了一系列函数，能让你在字符串中执行复杂的查找和替换操作。

---

## 参考链接：

1. [GNU正则表达式库](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
2. [正则表达式进阶教程](https://regexone.com/)
3. [Bash编程指南](http://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)

---