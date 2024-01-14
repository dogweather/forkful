---
title:    "C: 读取命令行参数"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么会读取命令行参数

在C编程中，读取命令行参数是一项非常有用的技能。它可以帮助程序员轻松地从终端接收输入数据，并且根据用户输入的不同来执行不同的代码。如果你想要创造一个交互式的程序，读取命令行参数就是必不可少的。

## 如何读取命令行参数

读取命令行参数的过程非常简单，只需要几行代码就可以搞定。让我们来看一个简单的例子：

```
#include <stdio.h> 

int main(int argc, char* argv[]) 
{ 
    // 打印第一个参数 
    printf("第一个参数是：%s\n", argv[1]); 
      
    return 0; 
} 
```

在上面的代码中，我们使用了两个参数来声明main函数，即argc和argv。argc表示命令行参数的数量，而argv则是一个指向这些参数的指针。从这里可以看出，命令行参数是以字符串的形式传递给程序的。因此，我们可以通过访问argv来获取命令行参数的值。在上面的例子中，我们通过`printf()`函数来打印第一个参数。

让我们来编译并运行这段代码，看看结果：

```
// 假设我们将这个程序命名为args
./args Hello

// 输出：
第一个参数是：Hello
```

从上面的例子可以看出，我们通过在程序文件后面添加参数“Hello”，就可以在程序中获取并使用该参数。这样就实现了一个简单的交互式程序。

## 深入学习读取命令行参数

在实际的程序中，我们可能需要读取多个命令行参数，并根据这些参数来执行不同的逻辑。为此，我们可以使用一个for循环来遍历argv数组，并使用`strcpy()`函数来复制参数值。此外，我们还可以使用`atoi()`函数来转换字符串类型的参数为整型。

让我们来看一个更复杂的例子：

```
#include <stdio.h>
#include <string.h> // 包含了strcpy函数
#include <stdlib.h> // 包含了atoi函数

int main(int argc, char* argv[])
{
    // 声明一个数组来存储参数值
    char params[3][10];

    // 使用for循环遍历argv数组
    for (int i = 1; i < argc; i++)
    {
        // 使用strcpy函数复制参数值到params数组中
        strcpy(params[i-1], argv[i]);
    }

    // 将字符串类型的第二个参数转换为整型
    int num = atoi(params[1]);

    // 根据参数执行不同的代码逻辑
    if (strcmp(params[0], "add") == 0)
    {
        int result = num + atoi(params[2]);
        printf("结果是：%d\n", result);
    }
    else if (strcmp(params[0], "subtract") == 0)
    {
        int result = num - atoi(params[2]);
        printf("结果是：%d\n", result);
    }

    return 0;
}
```

在上面的例子中，我们定义了一个char型二维数组来存储参数值，并使用for循环和`strcpy()`函数来获取参数值。然后，我们通过`strcmp()`函数来比较参数值，并根据不同的情况执行不同的代码逻辑。此外，我们还使用`atoi()`函数将字符串类型的参数转换为整型。

让我们来编译并运行这段代码，看看结果：

```
// 假设我们将这个程序命名为math
./math add 5 2

// 输出：
结果是：7

./math subtract 10 3

// 输出：
结果是：7
```

从上面的例子可以看出，通过读取命令行参数，我们可以在程序中实现一些复杂