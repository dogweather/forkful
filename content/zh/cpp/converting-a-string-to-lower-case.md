---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 什么以及为什么?

转换字符串为小写，意味着将字符串中所有的大写字母转换为小写。程序员之所以这么做，是为了让文本处理和比较更容易，因为这样做可以消除大小写差异对结果的可能影响。

# 如何做

在C++中，使用`<algorithm>`库的`transform()`函数和`<cctype>`库的`tolower()`函数。看以下的示例:

```C++
#include <iostream>
#include <algorithm>
#include <cctype>
using namespace std;

int main() {
    string str = "Hello, World!";
    transform(str.begin(), str.end(), str.begin(), ::tolower);
    cout << str << endl;  // output: "hello, world!"
    return 0;
}
```
在每次运行时，代码会输出 "hello, world!"。这是因为`transform()`函数会对字符串中的每个字符应用`tolower()`函数。

# 深度探索

在C++的历史中，转换字符串到小写一直被视为基本操作，但具体的执行方式有所不同。一些老版本的C++可能使用指针和数组进行操作，但现代C++更倾向于使用标准库和算法。

在C++内，你也可以用`for`循环来对字符串进行小写转换。这种方式比`transform()`函数直观，但可能需要更多的代码。参考下面的示例:

```C++
#include <iostream>
#include <cctype>
using namespace std;

int main() {
    string str = "Hello, World!";
    for (char & c : str) c = tolower(c);
    cout << str << endl;  // output: "hello, world!"
    return 0;
}
```
# 参考文献

以下是一些有用的链接，可以提供更多关于C++字符串操作的信息:

- Cplusplus.com: <http://www.cplusplus.com/reference/string/string/>
- Stackoverflow.com: <https://stackoverflow.com/questions/313970/how-to-convert-stdstring-to-lower-case>
- Tutorialspoint.com: <https://www.tutorialspoint.com/cplusplus/cpp_strings.htm>