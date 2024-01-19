---
title:                "将字符串转化为大写"
html_title:           "Bash: 将字符串转化为大写"
simple_title:         "将字符串转化为大写"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 啥是啥？为啥呢？
字串大写就是把字串中所有字母都变成大写的格式。有些编程要求必须使用这样的格式，比如常量或者环境变量。

## 如何做？
在Bash中，我们可以使用`${string^^}`这一语句来实现字串大写。看下面的例子：

```Bash
#!/bin/bash
string="hello, world"
echo ${string^^}
```
运行结果：

```Bash
HELLO, WORLD
```
所有的字母都变成了大写。

## 深入了解
${string^^}在Bash 4.0版本中被引入的，所以在早期的Bash版本中是不能使用的。在早期的Bash版本或者其它的shell中，我们可以通过管道和tr命令把字串变大写：

```Bash
string="hello, world"
echo $string | tr 'a-z' 'A-Z'
```

对于某些需要特殊处理的情况，例如只希望把首字母变大写，那就得用另一种方式，具体实现会麻烦一些：

```Bash
string="hello, world"
echo ${string^}
```

## 参考资料
1. Bash的 ${parameter^^} 扩展语法: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion
2. Bash tr命令: https://linux.die.net/man/1/tr
3. Bash 的 ${parameter^} 扩展语法: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion