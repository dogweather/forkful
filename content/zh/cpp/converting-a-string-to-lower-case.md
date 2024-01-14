---
title:                "C++: 将字符串转换为小写"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

为什么：在编程中，我们经常需要处理字符串。有时候，我们需要将字符串转换为小写形式，这样我们可以进行比较或者其他操作。因此，了解如何将字符串转换为小写形式是很有用的。

如何：为了将字符串转换为小写形式，我们可以使用C++的transform函数和tolower函数。首先，我们需要包含<algorithm>头文件和<cctype>头文件。然后，我们可以定义一个字符串并使用transform函数将其转换为小写形式。在括号中，我们需要传入字符串的开头和结尾，以及一个tolower函数作为第三个参数。最终，我们可以使用cout输出转换后的字符串。下面是一个简单的示例：

```C++
#include <iostream>
#include <algorithm>
#include <cctype>

using namespace std;

int main() {
    string s = "HELLO WORLD";
    transform(s.begin(), s.end(), s.begin(), tolower);
    cout << s << endl;
    return 0;
}
```

输出将会是 “hello world”。

深入了解：在深入了解字符串转换为小写的内部原理之前，我们需要知道ASCII码表。ASCII码表是一个标准，它将每个字母、数字和特殊字符都映射到一个数字值。在C++中，我们可以使用cctype头文件中的函数来处理ASCII码。tolower函数就是其中之一，它将大写字母转换为相应的小写字母的ASCII码。使用transform函数可以遍历字符串并使用tolower函数对每个字符进行转换。最后，使用iostream头文件中的cout输出转换后的字符串。

相关链接： -https://en.cppreference.com/w/cpp/string/byte/tolower -https://en.cppreference.com/w/cpp/algorithm/transform

看看这些链接，学习更多有关字符串转换为小写的细节。