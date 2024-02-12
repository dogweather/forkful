---
title:                "使用调试器"
aliases: - /zh/python/using-a-debugger.md
date:                  2024-01-26T04:09:21.337755-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用调试器"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/using-a-debugger.md"
---

{{< edit_this_page >}}

## 什么和为什么？
“使用调试器”是指逐步执行您的Python代码以发现错误和理解行为的过程。我们之所以这么做，是因为它比仅仅猜测问题发生在哪里要容易得多，而且它可以帮助我们省下数小时的打印语句炼狱。

## 如何操作：
让我们来详细了解使用`pdb`，Python内置的调试器。想象一下，有一个文件`buggy.py`，里面隐藏着一个隐蔽的错误：

```Python
def add_one(number):
    result = number ++ 1
    return result

print(add_one(7))
```

运行这个脚本时，您期待的是`8`，但它却抛出了一个语法错误。是时候使用调试器了！

在终端中运行：
```bash
python -m pdb buggy.py
```

您将进入调试器，界面如下：
```Python
> /path_to_file/buggy.py(1)<module>()
-> def add_one(number):
```

使用`l(ist)`查看更多代码，`n(ext)`跳到下一行，或`c(ontinue)`继续运行脚本。当您遇到错误时，`pdb`会停下来并让您进行检查。

在您将`number ++ 1`更正为`number + 1`之后，重启调试器以测试修复。
记住，朋友们不会让朋友们无净编码。已经说得够多了。

## 深入探讨
在编程的黑暗时代（也就是集成开发环境或IDE普及之前），调试器通常是您在文本编辑器外使用的独立工具。它们通过允许程序员检查软件在各个执行点的状态来拯救世界。

截至2023年，Python的`pdb`不是唯一的解决方案。人们可能会使用像PyCharm或Visual Studio Code这样的IDE，这些IDE内置了自己的漂亮调试器。这些添加了如用点击而非输入神秘命令来设置断点的便捷功能。

还有`ipdb`，一个可以通过pip安装的包，它将`IPython`的好处带到了调试中。它就像是加强版的`pdb`，具有选项卡补全和语法高亮显示功能。

调试器在实现上也各不相同。有些能够在机器或字节码级别与程序执行紧密结合。其他一些，如许多高级语言调试器，通过在特殊环境下运行代码来监视变量状态和控制执行流程。

## 另见
想要了解Python自带调试器的完整信息，请查看：
- `pdb`文档：https://docs.python.org/3/library/pdb.html

如果您对替代品感兴趣，这些链接会对您有所帮助：
- `ipdb`仓库和使用指南：https://github.com/gotcha/ipdb
- 使用Visual Studio Code进行调试：https://code.visualstudio.com/docs/python/debugging
- PyCharm调试功能：https://www.jetbrains.com/help/pycharm/debugging-code.html

祝您愉快地捕获错误！
