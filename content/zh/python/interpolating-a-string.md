---
title:                "字符串插值"
date:                  2024-01-20T17:51:41.916147-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串插值"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么和为什么？)
字符串插值指的是在字符串中嵌入变量或表达式的过程。程序员这么做是为了使输出内容动态、灵活，更加地方便阅读和维护。

## How to: (怎么做：)
在Python中，可以使用f-string来实现字符串插值：

```Python
name = "张三"
age = 28
greeting = f"你好，{name}。你今年{age}岁。"
print(greeting)
```

输出结果：

```
你好，张三。你今年28岁。
```

还可以在花括号中直接进行计算：

```Python
item = "苹果"
quantity = 5
price_per_item = 3
message = f"总价是：{quantity * price_per_item}元。"
print(message)
```

输出结果：

```
总价是：15元。
```

## Deep Dive (深入研究)
字符串插值在Python的历史中有几个阶段。早期，使用`%`操作符或者`.format()`方法是常见的方法。例如：

```Python
name = "李四"
age = 30
greeting = "你好，%s。你今年%d岁。" % (name, age)
# 或者
greeting = "你好，{}。你今年{}岁。".format(name, age)
```

但是，从Python 3.6起，推出了f-string，官方名称为Formatted String Literals，因为它更简洁且运行效率高于前者。在内部实现上，f-string在运行时会被转换为一个字符串格式化操作。值得注意的是，f-string内的表达式在运行时被评估，使得嵌入动态表达式变得很直接。

## See Also (另请参阅)
- [官方文档关于Formatted String Literals](https://docs.python.org/3/reference/lexical_analysis.html#f-strings)
- [PEP 498 -- Literal String Interpolation](https://www.python.org/dev/peps/pep-0498/): 提案详细说明了f-string的引入。
- [Python字符串格式化教程](https://realpython.com/python-f-strings/): 对字符串格式化的实际应用进行了深入讲解。
