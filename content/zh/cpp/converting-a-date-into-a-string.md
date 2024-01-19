---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么&为什么?

日期转换为字符串意味着我们把日期数据（通常是数字或对象）转换为人们可以更好理解的文本格式。程序员这样做是为了使程序的输出更人性化和可读。

## 如何操作:

以下是使用C++中的 `strftime` 来转换日期到字符串的示例代码:

```C++
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(nullptr);
    char str[100];
    std::strftime(str, sizeof(str), "%Y-%m-%d %H:%M:%S", std::localtime(&t));
    
    std::cout << str << '\n';

    return 0;
}
```
输出结果将是当前日期和时间的格式化字符串，例如 "2022-05-31 20:15:30"。

## 深度剖析

1. **历史背景**: 在早期的编程语言中，日期通常表示为从某一特定日期（例如1970年1月1日）开始的秒数。这就需要转化为更易理解的格式，如 "2022年5月31日"。
2. **替代方案**: 除了 `strftime`, C++ 还有其它库如 `std::chrono` 和 `boost::date_time` 可以进行日期的格式化。
3. **实现细节**: 使用 `strftime` 接口可以自定义输出的格式。这个接口的参数包含一个日期和时间格式的模板字符串，例如 "%Y-%m-%d %H:%M:%S" 表示年-月-日 时:分:秒。

## 参考资料:

- [strftime-reference](http://www.cplusplus.com/reference/ctime/strftime/): 完整的格式化参数列表。
- [std::chrono library](https://en.cppreference.com/w/cpp/chrono): C++标准库的一部分，用于处理日期和时间。
- [boost::date_time library](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html): Boost库的一部分，提供了更丰富的日期和时间操作功能。