---
title:    "C: 比较两个日期"
keywords: ["C"]
---

{{< edit_this_page >}}

# 为什么

在日常工作中，我们经常会需要比较日期。比如，我们可能需要检查某个事件是否发生在某个特定的日期之前或之后，或者我们需要按照日期来排序数据。在这种情况下，比较两个日期就会变得非常重要。通过比较两个日期，我们可以轻松地对日期进行排序、过滤或查找。

# 如何进行比较两个日期

在C语言中，比较两个日期需要用到一些关键的函数和操作符。首先，我们需要使用C语言的内置函数 `difftime()` 来计算两个日期之间的差值。然后，我们可以使用 `if` 条件语句来判断这个差值，并做出相应的处理。

下面是一个比较两个日期的示例代码：

```C
#include <stdio.h>
#include <time.h>

int main() {
    // 定义两个日期
    struct tm date1 = { .tm_year = 2021, .tm_mon = 9, .tm_mday = 1 };
    struct tm date2 = { .tm_year = 2020, .tm_mon = 9, .tm_mday = 1 };
    
    // 计算两个日期之间的差值
    time_t diff = difftime(mktime(&date1), mktime(&date2));
    
    // 判断差值并输出结果
    if (diff > 0) {
        printf("第一个日期在第二个日期之后\n");
    } else if (diff < 0) {
        printf("第一个日期在第二个日期之前\n");
    } else {
        printf("两个日期相同\n");
    }
    
    return 0;
}
```

运行这段代码，你会得到以下输出结果：

```
第一个日期在第二个日期之后
```

# 深入比较两个日期

在深入比较两个日期之前，我们需要了解一下 `struct tm` 结构体。`struct tm` 结构体是C语言中用来表示日期和时间的标准结构体，它包含了年、月、日等各种成员变量。

比较两个日期的基本思路就是将两个日期都转换成 `struct tm` 结构体，然后使用 `difftime()` 函数来计算差值。这样就可以得到两个日期之间的具体差值（单位为秒），从而可以做出相应的判断。

另外，你也可以使用C语言中的 `strcmp()` 函数来直接比较两个日期的字符串表示，但这种方法并不是很可靠，因为它只能比较字符串的大小，并不能准确地比较两个日期的先后顺序。

# 参考资料

- [C语言中的日期和时间操作](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [C语言内置函数difftime()](https://www.programiz.com/c-programming/library-function/time/difftime)
- [C标准库函数strcmp()](https://www.runoob.com/cprogramming/c-function-strcmp.html)

# 参见

- [如何在C语言中格式化日期和时间](https://github.com/Brightest-Sunshine/blog-posts/blob/main/c-format-date-time.md)
- [在C语言中使用结构体](https://github.com/Brightest-Sunshine/blog-posts/blob/main/c-structs.md)
- [使用if条件语句来做出判断](https://github.com/Brightest-Sunshine/blog-posts/blob/main/c-if-else-statements.md)