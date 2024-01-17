---
title:                "提取子字符串"
html_title:           "C++: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

＃＃ "是什麼和為什麼？"：
提取子字符串是指從一個字符串中提取出特定部分的過程。程序員這樣做的原因是為了更有效地操作字符串數據，例如搜索和替換特定內容。

＃＃ "如何："
假設有一個字符串 ```"Hello World!"```，我們想要提取出其中的 ```"World"``` 部分，可以使用以下代碼：

```
#include <iostream>

using namespace std;

int main() {
    string str = "Hello World!";
    string subStr = str.substr(6, 5); // 從索引 6 開始提取 5 個字符
    cout << subStr; // 輸出 "World"
    return 0;
}
```

＃＃ "深入探討"
（1）歷史背景：提取子字符串的概念早在早期的計算機程序設計中就存在。隨著數據處理和信息檢索需求的增加，提取子字符串的功能也變得更加重要。
（2）替代方法：除了使用 ```substr()``` 函數，還可以使用其他方法提取子字符串，例如使用指針或遍歷字符串進行提取。
（3）實現細節：```substr()``` 函數的實現原理是基於字符串的指標操作，可以訪問原始字符串的特定部分，並將其拷貝到新的字符串中。

＃＃ "參考資料"
- [C++ substr() 函数](https://www.runoob.com/cplusplus/cpp-string-erase.html)
- [字符串的常见操作](https://www.geeksforgeeks.org/string-class-in-cpp-stl/)