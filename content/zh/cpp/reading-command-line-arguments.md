---
title:    "C++: 读取命令行参数"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么要读取命令行参数

读取命令行参数是一项在C++编程中非常有用的技能。通过读取命令行参数，我们可以在运行程序时向程序传递一些额外的信息，从而使程序可以根据不同的参数执行不同的操作。这样可以让程序更加灵活和智能，节省开发时间。

# 如何读取命令行参数

在C++中，我们可以通过使用main函数的参数argc和argv来读取命令行参数。argc是一个表示参数个数的整数，而argv是一个指针数组，它存储了每个参数的值。让我们来看一个简单的例子：

```C++
#include <iostream>

int main(int argc, char* argv[]) {
    // 打印参数个数
    std::cout << "参数个数：" << argc << std::endl;

    // 打印每个参数的值
    for (int i = 0; i < argc; i++) {
        std::cout << "参数" << i << "：" << argv[i] << std::endl;
    }

    return 0;
}
```

假设我们将上述代码保存为"args.cpp"，然后在命令行中输入以下命令：

```bash
g++ args.cpp -o args # 编译程序
./args param1 param2 param3 # 运行程序并传入三个参数
```

程序的输出将会是：

```
参数个数：4
参数0：./args
参数1：param1
参数2：param2
参数3：param3
```

这里我们可以看到，argc的值为4，其中包括程序本身的名称和传入的三个参数的值。

# 深入了解命令行参数

除了使用argc和argv读取命令行参数外，C++标准库中也提供了getopt函数来帮助我们处理命令行参数。getopt函数可以解析命令行参数并将其存储在一个结构体中，从而方便我们读取和使用。让我们来看一个例子：

```C++
#include <iostream>
#include <unistd.h> // 包含getopt头文件

int main(int argc, char* argv[]) {
    // 传入的参数格式：-a value -b value2
    int opt;
    // 定义一个保存参数值的变量
    int a, b;

    // 循环调用getopt函数来解析参数
    while ((opt = getopt(argc, argv, "a:b:")) != -1) {
        switch(opt) {
            case 'a':
                // 使用atoi函数将参数值转换为整数
                a = atoi(optarg);
                break;
            case 'b':
                // 使用atoi函数将参数值转换为整数
                b = atoi(optarg);
                break;
            default:
                // 如果命令行参数不符合指定的格式，输出提示信息并退出程序
                std::cout << "命令行参数格式不正确！" << std::endl;
                return 1;
        }
    }

    // 打印参数值
    std::cout << "参数a的值为：" << a << std::endl;
    std::cout << "参数b的值为：" << b << std::endl;

    return 0;
}
```

同样假设我们将上述代码保存为"getopt.cpp"，然后在命令行中输入以下命令：

```bash
g++ getopt.cpp -o getopt # 编译程序
./getopt -a 10 -b 20 # 运行程序并传入参数
```

程序的输出将会是：

```
参数a的值为：10
参数b的值为：20
```

通过使用getopt函数，我们可以指定命令行参数的格式，从而更加灵活地处理参数。

# 参考链接

- [C++命令行参数详解](https://www.jianshu.com/p/096c3f9e0f07)
- [Using getopt in C/C++](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)