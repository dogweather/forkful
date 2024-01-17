---
title:                "比较两个日期"
html_title:           "Ruby: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

##什么是日期比较 & 为什么程序员要这么做?
日期比较是指将两个日期相互比较，以确定哪个日期早于或晚于另一个日期。程序员通常会进行日期比较来对数据进行排序、筛选或分组，以及进行日期相关的计算和分析。

##如何进行日期比较:
```ruby
# 比较两个日期
date1 = Date.parse("2020-01-01")
date2 = Date.parse("2020-01-05")
date1 <=> date2 #=> -1

# 判断两个日期是否相等
date1 = Date.parse("2020-01-01")
date2 = Date.parse("2020-01-01")
date1 == date2 #=> true

# 比较日期前后
date1 = Date.parse("2020-01-01")
date2 = Date.parse("2020-01-05")
date1 < date2 #=> true
```

##深入了解:
日期比较已经成为计算机编程中常见的操作。它的历史可以追溯到早期的计算机语言，如COBOL和Fortran。除了使用Ruby内置的Date类进行比较外，程序员也可以使用第三方库，如Chronic或Date Calc来更方便地处理日期比较。

##相关资料:
- Ruby Date类文档: https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html
- Chronic库: https://github.com/mojombo/chronic
- Date Calc库: http://datecalc.rubyforge.org/