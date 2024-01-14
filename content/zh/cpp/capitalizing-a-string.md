---
title:    "C++: 将字符改为大写"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

为什么：首先我们需要明确为什么我们要对一个字符串进行大写。通常情况下，我们可能需要将字符串格式化成标题、打印日志或者进行字符串比较等操作。

怎么做：现在来看看如何用C ++代码来实现字符串大写。首先，我们需要定义一个字符数组来存储我们要处理的字符串，然后使用for循环来遍历数组中的每一个字符，并使用toupper()函数来将小写字母转换为大写字母。最后，输出转换后的字符串。

```C++
#include <iostream>
#include <cctype>

using namespace std;

int main() {
    //定义字符串
    char str[] = "hello world";
    
    //遍历数组并转换字符
    for (int i = 0; i < strlen(str); i++) {
        str[i] = toupper(str[i]);
    }
    
    //输出结果
    cout << str << endl;
    
    return 0;
}
```
输出结果：
HELLO WORLD

深入了解：为了更好地理解字符串大写的处理过程，让我们来深入了解一下toupper()函数。这个函数是C++标准库中的一个函数，它的作用是将小写字母转换为大写字母。它接受一个字符参数，并返回对应的大写字符。在上面的例子中，我们使用了for循环来遍历字符数组，并对每一个字符都调用了toupper()函数。值得注意的是，如果字符本身就是大写字母，则不会被改变。

看看其他的字符处理函数：
- tolower()：将大写字母转换为小写字母。
- isupper()：判断一个字符是否为大写字母。
- islower()：判断一个字符是否为小写字母。

所以，有了这些强大的字符处理函数，我们能够轻松地进行字符串大小写转换和其他字符处理操作。

参考文章：
- [toupper()函数](https://www.cplusplus.com/reference/cctype/toupper/)
- [C++字符串处理教程](https://www.runoob.com/cplusplus/cpp-string.html)

另请参阅：
请查看C++标准函数库中的其他有用的函数，以加强您对字符处理的理解。