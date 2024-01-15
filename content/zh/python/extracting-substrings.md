---
title:                "提取子字符串"
html_title:           "Python: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

提取子字符串是在处理字符串时经常需要使用的功能。通过提取子字符串，我们可以从一个较长的字符串中获取我们需要的特定部分，从而更方便地处理字符串数据。

## 如何做

```Python
# 使用切片操作提取子字符串
my_string = "Hello world!"
substring = my_string[1:5]

# 输出：
print(substring) # "ello"
```

```Python
# 使用split函数提取子字符串
my_string = "I love Python programming!"
substring = my_string.split("Python")[1]

# 输出：
print(substring) # " programming!"
```

```Python
# 使用正则表达式提取子字符串
import re
my_string = "I have 3 cats and 2 dogs"
substring = re.findall(r'\d+', my_string)

# 输出：
print(substring) # ['3', '2']
```

## 深入了解

通过切片操作，我们可以根据索引位置来提取子字符串。使用split函数可以将字符串按照指定的分隔符分割成多个子字符串，并选择我们需要的子字符串。而使用正则表达式则可以更加灵活地提取符合特定规则的子字符串。

## 参考链接

- [Python字符串方法](https://www.runoob.com/python3/python3-string.html)
- [Python正则表达式指南](https://www.runoob.com/regexp/regexp-tutorial.html)
- [Python字符串切片操作指南](https://www.jianshu.com/p/b65035a23057)