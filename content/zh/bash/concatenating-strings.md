---
title:                "Bash: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

Bash编程是一种流行的编程语言，它可以用来编写简单的脚本，也可以用来完成复杂的任务。而字符串连接是Bash编程中经常用到的技巧，它可以帮助我们将多个字符串合并成一个更大的字符串。这个技术可以在处理文本数据、生成报告和其他许多应用中发挥重要的作用。

## 如何

在Bash中，我们可以使用“+”符号来连接两个字符串。让我们来看一个例子：

```Bash
first_name="John"
last_name="Smith"
full_name=$first_name+$last_name
echo $full_name
```

输出：John+Smith

这里我们定义了两个变量，一个是first_name，一个是last_name，然后使用“+”符号连接它们并赋值给full_name变量。最后通过echo命令打印出full_name变量的值，即John+Smith。

我们也可以通过在两个字符串中间使用空格来连接它们：

```Bash
first_name="John"
last_name="Smith"
full_name=$first_name" "$last_name
echo $full_name
```

输出：John Smith

在这个例子中，我们通过在$first_name和$last_name变量中间加入一个空格来连接它们，最终输出的结果就是两个名字中间有一个空格。

我们还可以使用变量和普通的文本来连接字符串：

```Bash
name="John"
greeting="Hello $name, welcome to my blog!"
echo $greeting
```

输出：Hello John, welcome to my blog!

这里我们定义了一个变量name为John，然后同时在greeting变量中使用了该变量和普通文本。结果输出了一条问候语，其中包含了John的名字。

## 深入了解

在Bash编程中，字符串连接是一个比较简单的技巧。然而，在实际应用中，我们也可以利用很多其他的方法来完成字符串的连接。例如使用+=运算符来连接字符串：

```Bash
first_name="John"
last_name="Smith"
first_name+=$last_name
echo $first_name
```

输出：JohnSmith

在这个例子中，我们使用+=运算符来连接两个字符串，并将结果赋值给first_name变量。最终输出的结果就是JohnSmith，即两个名字合并在一起。

除了+和+=运算符，我们还可以使用printf命令来格式化字符串并连接它们。

在这里，我们使用%s占位符来表示字符串，并使用“%s%s”来连接两个字符串：

```Bash
first_name="John"
last_name="Smith"
full_name=$(printf "%s%s" $first_name $last_name)
echo $full_name
```

输出：JohnSmith

这个技巧同样也可以用来连接多个字符串，只需在printf命令中添加相应的%s占位符即可。

## 参考链接

- [Bash字符串连接](https://www.linuxjournal.com/content/bash-variable-manipulation-and-string-concatenation)
- [Bash字符串连接方法详解](https://linuxize.com/post/how-to-concatenate-strings-in-bash/)
- [深入理解Bash中的字符串连接](https://www.tecmint.com/working-with-strings-in-bash/)

## 参见

其他有关Bash编程的相关文章：

- [Bash编程入门指南](https://www.linuxjournal.com/content/introduction-bash-shell-scripting)
- [Bash常用命令大全](https://www.linuxize.com/post/bash-commands/)