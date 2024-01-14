---
title:    "C++: “打印调试输出”"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

# 为什么

当我们在编写C++程序时，可能会遇到各种各样的问题。这些问题可能导致我们的程序运行出错或者出现意外的结果。在这种情况下，输出调试信息是非常有用的，可以帮助我们找出程序中的错误并进行修复。因此，学会如何输出调试信息是非常重要的。

## 如何做

要输出调试信息，我们可以使用C++中的标准输出函数`cout`。下面是一个简单的例子，演示了如何使用`cout`来输出调试信息：

```C++
#include <iostream>

using namespace std;

int main() {
    int num = 10;

    // 输出调试信息
    cout << "变量num的值为：" << num << endl;

    return 0;
}
```

上面的代码中，我们使用了`cout`来输出一个变量的值。使用`<<`运算符可以将文本和变量的值连接起来输出。在这个例子中，我们输出了一段文本“变量num的值为：”，然后用`num`变量的值替换`<<`后面的`num`。`endl`表示换行，用来使输出更加清晰易读。

运行上面的代码，我们就可以在控制台看到输出结果：

```
变量num的值为：10
```

从这个例子中，我们可以看到输出调试信息非常简单。我们可以在程序中随时添加调试输出来帮助我们理解程序的执行过程。

## 深入探讨

除了使用`cout`输出调试信息，我们还可以使用C++中的`assert`函数来进行断言调试。断言是一种测试程序是否满足特定条件的方法，如果断言失败，程序就会终止并输出错误信息。下面是一个使用`assert`的例子：

```C++
#include <iostream>
#include <cassert>

using namespace std;

int main() {
    int num = 5;

    // 使用断言调试
    assert(num >= 10);

    // 继续执行代码
    cout << num << endl;

    return 0;
}
```

在这个例子中，我们断言变量`num`的值必须大于等于10。因为`num`的值为5，所以断言失败，程序就会终止并输出错误信息。这样我们就可以知道程序在哪里出现了问题，并进行相应的调试。

输出调试信息可以帮助我们更好地理解程序的执行过程，找出程序中的错误。但是需要注意的是，调试信息只是用来帮助我们调试程序，不应该出现在最终的代码中。所以，在正式发布的程序中最好将所有的调试信息删除或者注释掉。

# 参考文献

- [C++调试技巧](https://blog.csdn.net/wxl1555/article/details/7297873)
- [C++断言调试](https://blog.csdn.net/u012709028/article/details/50213049)

# 参见

- [C++开发入门指南](https://github.com/OuroborosOfBorg/cpp-guide)
- [深入理解C++](https://github.com/taowen/awesome-cpp)
- [C++标准库参考手册](https://en.cppreference.com/w/)