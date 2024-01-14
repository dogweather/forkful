---
title:    "Bash: 编写测试"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## 为什么

编写测试是提高代码质量不可或缺的步骤。通过编写测试，您可以确保代码在未来的更改中仍然可靠运行，减少出错的可能性，并节省时间和精力。

## 如何

编写测试可以使用Bash编程语言轻松地实现。您可以按照以下步骤来编写测试：

1. 首先，创建一个新的Bash脚本文件，例如`test.sh`。
2. 在脚本中，使用`#!/bin/bash`来定义Bash解释器。
3. 使用`echo`命令来打印测试的名称，例如`echo "测试1"`。
4. 下面是一个示例代码块，它包含了一个简单的加法函数和测试它的功能。

```Bash
#!/bin/bash

# 定义加法函数
function add {
    echo "请输入两个数字："
    read num1
    read num2
    sum=$((num1+num2)) # 使用双括号来执行加法运算
    echo "总和是： $sum"
}

# 测试函数：测试加法函数的功能
function test_add {
    add # 调用add函数
    if [ "$sum" -eq $((5+10)) ]; then # 使用条件语句来判断加法是否正确
        echo "测试通过！"
    else
        echo "测试失败！"
    fi
}

# 调用测试函数来运行测试
test_add
```

5. 在终端中，使用`bash test.sh`来运行测试脚本。

运行示例：

```
测试1
请输入两个数字：
5
10
总和是： 15
测试通过！
```

## 深入探讨

编写测试的关键是确保测试覆盖所有可能的代码路径。您可以使用不同的函数和条件语句来测试不同的代码功能。此外，您还可以使用`exit status`来检查是否有错误发生。

例如，在前面的加法函数的例子中，我们可以修改`add`函数来检查输入的数字是否为0，并返回相应的`exit status`：

```Bash
function add {
    echo "请输入两个数字："
    read num1
    read num2
    if [ "$num1" -eq 0 ] || [ "$num2" -eq 0 ]; then # 检查输入的数字是否为0
        echo "输入错误！"
        return 1 # 返回exit status 1来表示错误
    fi
    sum=$((num1+num2)) # 使用双括号来执行加法运算
    echo "总和是： $sum"
    return 0 # 返回exit status 0来表示成功
}
```

然后，在测试函数中，我们也需要修改来检查`add`函数的exit status：

```Bash
function test_add {
    add # 调用add函数
    if [ $? -eq 0 ]; then # 使用$?来获取上一个命令的exit status
        echo "测试通过！"
    else
        echo "测试失败！"
    fi
}
```

## 另请参阅

- [Bash程序设计教程](www.linuxforbeginners.info/batch_scripting)
- [Bash编程入门](https://www.tutorialspoint.com/unix/shell_scripting.htm)
- [Bash文档](https://www.gnu.org/software/bash/manual/html_node/index.html)