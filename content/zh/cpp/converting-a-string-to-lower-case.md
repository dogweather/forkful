---
title:    "C++: 将字符串转换为小写"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

为什么：当我们在处理大量文本数据时，经常会遇到需要将文本转换为小写的需求。这样可以保证不同文本的一致性，方便后续的处理和分析。

如何做：在C++中，可以使用transform函数来实现字符串的小写转换。代码示例如下所示：

```C++ 
#include <iostream> 
#include <string> 
#include <algorithm> 

using namespace std; 

int main() { 
    string text = "HELLO WORLD"; 

    // 将字符串转换为小写 
    transform(text.begin(), text.end(), text.begin(), ::tolower); 

    // 输出转换后的结果 
    cout << text << endl; 

    return 0; 
} 
```

输出结果为：

```
hello world
```

深入探讨：在上面的代码中，我们使用了C++中的transform函数来实现字符串的小写转换。该函数接受三个参数，分别是要转换的字符串的起始位置、结束位置和转换后的字符串的起始位置。在第四个参数中，我们使用了::tolower来指定转换的方法，这是C++中内置的一个函数。

此外，在transform函数中，我们还可以自定义转换的方法。例如，我们可以使用lambda表达式来指定转换的具体方法，代码示例如下：

```C++ 
#include <iostream> 
#include <string> 
#include <algorithm> 

using namespace std; 

int main() { 
    string text = "HELLO WORLD"; 

    // 使用lambda表达式来指定转换的方法 
    transform(text.begin(), text.end(), text.begin(), [](unsigned char c){ return std::tolower(c); }); 

    // 输出转换后的结果 
    cout << text << endl; 

    return 0; 
} 
```

输出结果为：

```
hello world
```

最后，值得注意的是，在进行字符串转换时，我们需要注意字符的编码。例如，在处理中文文本时，可能需要使用其他的转换方法来保证准确性。

参考链接：
- [C++ string处理函数transform()的用法](https://www.imooc.com/wenda/detail/509277)
- [C++ primer Plus - 转换大写和小写](https://www.runoob.com/cplusplus/cpp-string-toupper-tolower.html)

## 参考链接

- [C++ string处理函数transform()的用法](https://www.imooc.com/wenda/detail/509277)
- [C++ primer Plus - 转换大写和小写](https://www.runoob.com/cplusplus/cpp-string-toupper-tolower.html)