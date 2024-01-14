---
title:                "Fish Shell: 使用标准错误输出写入程序"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 为什么

在编程中，有时候我们想要输出一些不是正常的结果，或者是为了调试我们的代码。这时候，将结果打印到标准错误（standard error）就很有用了。

# 如何

编写Fish Shell脚本时，使用标准输出（standard output）和标准错误（standard error）是非常常见的。下面是一个示例代码，展示了如何将结果打印到标准错误。

```Fish Shell 
echo "这是一个标准输出"  # 打印到标准输出
echo "这是一个标准错误" >&2 # 将结果打印到标准错误
```

运行这个脚本，我们可以看到标准输出和标准错误分别输出了不同的内容，这可以帮助我们区分出不同的信息。

```
这是一个标准输出
这是一个标准错误
```

# 深入探讨

当我们将结果打印到标准错误时，实际上是将这些信息发送到stderr流。这通常显示为红色的文字，表示这是一个错误信息。相比之下，标准输出通常以标准的黑色文字显示。将结果打印到标准错误可以帮助我们区分出不同的输出，从而更好地调试我们的代码。

# 参考链接

- [Fish Shell官方网站](https://fishshell.com/)
- [官方文档：输入/输出](https://fishshell.com/docs/current/tutorial.html#inputandoutput)
- [官方文档：有用的技巧](https://fishshell.com/docs/current/tutorial.html#usefultechniques)
- [官方文档：变量和流](https://fishshell.com/docs/current/tutorial.html#variablesandstreams)

# 参考链接