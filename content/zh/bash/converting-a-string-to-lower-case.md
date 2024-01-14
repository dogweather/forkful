---
title:                "Bash: 将字符串转换为小写"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为何要将字符串转换为小写

在Bash编程中，经常需要将字符串转换为小写格式。这样做的原因可能是为了数据标准化或者与其他字符串比较时的方便。无论是什么原因，将字符串转换为小写是一个常见的编程任务，今天我们就来看看如何在Bash中实现这一操作。

## 如何进行字符串小写转换

要在Bash中将字符串转换为小写格式，最简单的方法是使用内建命令`tr`。在下面的代码示例中，我们将使用`tr`命令来将字符串`Hello World`转换为小写，并将结果存储在变量`lowercase`中。

```Bash
# 定义要转换的字符串
my_string="Hello World"

# 使用tr命令将字符串转换为小写
lowercase=$(echo "$my_string" | tr '[:upper:]' '[:lower:]')

# 打印转换后的结果
echo $lowercase
```

代码执行结果：

```Bash
hello world
```

通过上面的代码，我们可以看到`tr`命令可以很方便地将字符串转换为小写，它接受两个参数，第一个参数是要转换的字符集，第二个参数是转换后的字符集。在这个例子中，我们将`[:upper:]`作为第一个参数，表示要转换的字符集是所有大写字母，`[:lower:]`作为第二个参数，表示转换后的字符集是所有小写字母。

除了使用`tr`命令，我们也可以使用Bash内建变量`${parameter,,}`来实现字符串小写转换。下面的代码示例与上面的示例功能相同，只是使用了不同的方法。

```Bash
# 定义要转换的字符串
my_string="Hello World"

# 使用内建变量进行转换
lowercase=${my_string,,}

# 打印转换后的结果
echo $lowercase
```

## 深入了解字符串小写转换

在上面的示例中，我们使用了`tr`命令和内建变量来实现转换。但是，你可能会注意到，如果字符串中既包含大写字母又包含小写字母，使用这两种方法转换后的结果会有所不同。`tr`命令只会转换字符串中的大写字母，而内建变量则会将所有字符都转换为小写。因此，在选择使用哪种方法时，需要根据具体的需求进行取舍。

除了`tr`命令和内建变量，我们还可以使用Bash中的正则表达式来实现字符串小写转换。下面的代码示例展示了如何利用正则表达式来将字符串中的所有字母转换为小写。

```Bash
# 定义要转换的字符串
my_string="Hello World"

# 使用正则表达式进行转换
lowercase=$(echo "$my_string" | sed -E 's/([A-Z])/\L\1/g')

# 打印转换后的结果
echo $lowercase
```

除了正则表达式，在Bash中也可以使用`awk`命令来实现字符串小写转换。下面的代码示例展示了如何将字符串中的所有字母转换为小写。

```Bash
# 定义要转换的字符串
my_string="Hello World"

# 使用awk命令进行转换
lowercase=$(echo "$my_string" | awk '{ print tolower($0) }')

# 打印转换后的结果
echo $lowercase
```

## 参考链接

- [Bash文档](https://www.gnu.org/software/bash/manual/)
- [tr命令文档](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Bash内建变量文档](https://www.gnu.org/software/bash/manual/html_node/B