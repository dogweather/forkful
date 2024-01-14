---
title:    "C: 在计算机编程中，这篇文章的标题是：“计算未来或过去的日期”。"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 为什么
在编程中，我们经常需要计算未来或过去的日期。这有助于我们在各种情况下处理时间相关的任务，如日程安排，统计数据和时间限制。使用C语言编程，我们可以轻松地计算未来或过去的日期，并将其应用于各种程序。

# 如何实现
首先，我们需要包含<time.h>头文件，以便使用C语言提供的日期和时间函数。然后，我们需要定义一个结构体来存储日期，包括年，月和日。接下来，我们使用函数mktime（）来将日期转换为自1970年1月1日以来的秒数，以便进行计算。最后，我们使用函数gmtime（）将计算后的日期转换为易于阅读的格式。

```C
#include <stdio.h>
#include <time.h>

int main(){
  // 定义结构体存储日期
  struct tm date;
  int year, month, day;
  
  // 获取用户输入的日期
  printf("请输入年份：");
  scanf("%d", &year);
  printf("请输入月份：");
  scanf("%d", &month);
  printf("请输入日期：");
  scanf("%d", &day);
  
  // 将日期转换为秒数
  date.tm_year = year-1900;  // tm_year以1900年为起始
  date.tm_mon = month -1;    // tm_mon从0开始，1月为0
  date.tm_mday = day;
  time_t seconds = mktime(&date);
  
  // 计算未来5天的日期并输出
  printf("未来5天的日期是：\n");
  for (int i = 1; i <= 5; i++){
    seconds += 24 * 60 * 60;  // 秒数加一天
    date = *gmtime(&seconds); // 将秒数转换为易于阅读的日期格式
    printf("%d月%d日\n", date.tm_mon+1, date.tm_mday);
  }
  
  return 0;
}
```
输出：

请输入年份：2021
请输入月份：7
请输入日期：15
未来5天的日期是：
7月16日
7月17日
7月18日
7月19日
7月20日

# 深入了解
在上面的代码示例中，我们使用了C语言中一些基本的日期和时间函数来计算未来的日期。但是，我们还可以使用其他功能来处理更复杂的日期计算，如计算两个日期之间的天数差，检查是否为闰年等等。

有时候，我们可能需要根据不同的地区和文化来计算日期。这就要用到C语言提供的国际化函数来处理不同的日期格式和日历系统。

此外，我们还可以结合使用C语言和其他语言来实现更强大的日期计算功能。比如，结合使用C语言和SQL语言可以处理日期在数据库中的存储和查询，结合使用C语言和网页开发语言可以实现日历功能等等。

# 参考链接
- <https://www.runoob.com/cprogramming/c-function-mktime.html>
- <https://www.tutorialspoint.com/c_standard_library/c_function_mktime.htm>
- <https://www.ibm.com/docs/zh/cics-ts/5.4?topic=languages-ctime-ref-reference-ctime-date-convert-function-c>
- <https://www.gnu.org/software/libc/manual/html_node/Initializing-the-Current-Time.html#Initializing-the-Current-Time>
- <https://www.cnblogs.com/chen2886/p/4708829.html>