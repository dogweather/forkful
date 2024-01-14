---
title:    "Arduino: 读取命令行参数"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

为什么会有人想要学习读取命令行参数呢？因为在编程中，我们经常会需要用户输入一些信息来执行不同的操作。通过读取命令行参数，我们可以方便地接收用户输入，并根据不同的参数执行不同的代码。这对于想要扩展自己的编程能力的人来说是非常有用的。

## 如何

如果你是一个Arduino编程的初学者，你可能会觉得读取命令行参数很复杂。但实际上，它并不难，只需要按照以下步骤进行：

1. 首先，我们需要在代码中声明一个main()函数，它将是我们程序执行的入口点。
2. 在main()函数的括号内，我们可以声明两个参数：一个是整型的argc，表示命令行参数的个数；另一个是字符型的argv，表示命令行参数的数组。
3. 在代码的运行过程中，命令行参数将会被自动存储在这两个变量中，并可以通过它们来读取和使用。

下面是一个简单的例子，展示如何读取命令行参数并显示在串口监视器上：

```Arduino
void main(int argc, char** argv){
  //读取第一个参数，并将其转换为整型
  int num = atoi(argv[1]);
  //在串口监视器上显示命令行参数
  Serial.print("您输入的数字是：");
  Serial.println(num);
}
```

假设我们将上面的代码上传到Arduino板，然后在串口监视器中输入命令行参数`123`，我们将会看到以下输出：

```
您输入的数字是：123
```

通过这种方式，我们就可以方便地读取用户输入的参数并进行处理。

## 深入探讨

除了上面提到的方法外，我们还可以通过C语言中的`getopt()`函数来读取命令行参数。这个函数可以帮助我们更方便地解析命令行参数，并返回对应的值。下面是一个使用`getopt()`函数的例子：

```Arduino
void main(int argc, char** argv){
  //声明一个字符型变量，存储getopt()返回的结果
  int opt;
  //循环读取用户输入的每个选项
  while ((opt = getopt(argc, argv, "abc:")) != -1) {
    //判断用户输入的选项，并进行相应的操作
    switch (opt) {
      case 'a':
        Serial.println("您输入了选项a");
        break;
      case 'b':
        Serial.println("您输入了选项b");
        break;
      case 'c':
        //读取并转换为整型
        int num = atoi(optarg);
        //显示用户输入的值
        Serial.print("您输入的数字是：");
        Serial.println(num);
        break;
    }
  }
}
```

假设我们使用命令行参数`-a -c500`，则会得到以下输出：

```
您输入了选项a
您输入的数字是：500
```

需要注意的是，`getopt()`函数只能读取单个字符的选项，而不能读取带有参数的选项，例如`-c500`。如果需要读取带有参数的选项，可以使用C++中的`getopt_long()`函数。

## 参考链接

- [C++ getopt()函数详解](https://www.runoob.com/cprogramming/c-function-getopt.html)
- [C++ getopt_long()函数详解](https://www.runoob.com/w3cnote/cpp-getopt_long-function.html)
- [Arduino命令行参数读取教程](https://forum.arduino.cc/index.php?topic=384198.0)

## 参见

- [Arduino编程入门指南](https://github.com/Clement19891207/Getting-started-with-Arduino/blob/master/README.md)
- [Arduino官方文档](