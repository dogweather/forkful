---
title:                "Fish Shell: 连接字符串"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

# 为什么要使用 Fish Shell 连接字符串

字符串的连接是一个常见的编程任务，它可以帮助我们将多个字符串组合成一个更大的字符串。在 Fish Shell 中，我们可以使用简单的语法来完成这个任务，让我们来看看如何做到这一点吧！

# Fish Shell 中的连接字符串的方法

```Fish Shell
echo "Hello" "World"
```

输出：

```
Hello World
```

使用空格将两个字符串放在一起，Fish Shell 会自动将它们连接在一起。我们也可以使用一个空的 echo 命令来完成同样的任务：

```Fish Shell
echo
"Hello" "World"
```

输出：

```
Hello World
```

我们还可以将变量和字符串连接起来：

```Fish Shell
set name "Fish Shell"
echo "Welcome to" $name
```

输出：

```
Welcome to Fish Shell
```

# 深入了解连接字符串

在 Fish Shell 中，连接字符串的方法有很多种，我们可以使用单引号、双引号、拼接符号以及字符串插值来完成这个任务。使用单引号可以将字符串原封不动地输出，而双引号会将变量进行字符串插值，可以使用拼接符号将多个字符串连接起来。示例代码如下：

```Fish Shell
set name "Fish Shell"
echo 'Welcome to $name'
echo "Welcome to" $name
echo "Welcome to " $name
```

输出：

```
Welcome to $name
Welcome to Fish Shell
Welcome to Fish Shell
```

除了上述方法外，我们还可以使用字符串插值来连接字符串：

```Fish Shell
set name "Fish Shell"
echo "Welcome to $name"
```

输出：

```
Welcome to Fish Shell
```

总的来说，在 Fish Shell 中连接字符串是非常简单直观的，通过灵活运用不同的方法，我们可以轻松完成这一任务。

# 另请参阅

- [Fish Shell 文档：字符串](https://fishshell.com/docs/current/tutorial.html#strings)
- [Fish Shell 官方 Github 仓库](https://github.com/fish-shell/fish-shell)
- [Fish Shell 用户论坛](https://groups.google.com/forum/#!forum/fish-users)

# See Also

- [Fish Shell Documentation: Strings](https://fishshell.com/docs/current/tutorial.html#strings)
- [Official Fish Shell Github Repository](https://github.com/fish-shell/fish-shell)
- [Fish Shell User Forum](https://groups.google.com/forum/#!forum/fish-users)