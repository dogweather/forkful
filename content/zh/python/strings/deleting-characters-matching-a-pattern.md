---
date: 2024-01-20 17:42:55.145944-07:00
description: "\u5220\u9664\u5339\u914D\u6A21\u5F0F\u7684\u5B57\u7B26\u5C31\u662F\u627E\
  \u51FA\u5B57\u7B26\u4E32\u4E2D\u6EE1\u8DB3\u7279\u5B9A\u89C4\u5219\u7684\u90E8\u5206\
  \uFF0C\u5E76\u5C06\u5B83\u4EEC\u79FB\u9664\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\
  \u901A\u5E38\u662F\u4E3A\u4E86\u6570\u636E\u6E05\u6D17\u6216\u51C6\u5907\u6587\u672C\
  \u8FDB\u884C\u8FDB\u4E00\u6B65\u5904\u7406\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:44.869064-07:00'
model: gpt-4-1106-preview
summary: "\u5220\u9664\u5339\u914D\u6A21\u5F0F\u7684\u5B57\u7B26\u5C31\u662F\u627E\
  \u51FA\u5B57\u7B26\u4E32\u4E2D\u6EE1\u8DB3\u7279\u5B9A\u89C4\u5219\u7684\u90E8\u5206\
  \uFF0C\u5E76\u5C06\u5B83\u4EEC\u79FB\u9664\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\
  \u901A\u5E38\u662F\u4E3A\u4E86\u6570\u636E\u6E05\u6D17\u6216\u51C6\u5907\u6587\u672C\
  \u8FDB\u884C\u8FDB\u4E00\u6B65\u5904\u7406\u3002"
title: "\u5339\u914D\u6A21\u5F0F\u5220\u9664\u5B57\u7B26"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
删除匹配模式的字符就是找出字符串中满足特定规则的部分，并将它们移除。程序员这么做通常是为了数据清洗或准备文本进行进一步处理。

## How to: 怎样做
```python
import re

# 示例字符串
text = "找些数字123和特殊符号#$去除它们!"

# 删除所有数字
no_digits = re.sub(r'\d+', '', text)
print(no_digits)
# 输出: 找些数字和特殊符号#$去除它们!

# 删除特殊符号
no_symbols = re.sub(r'[^\w\s]', '', text)
print(no_symbols)
# 输出: 找些数字123和特殊符号去除它们
```

## Deep Dive 深入探索
删除匹配模式的字符在编程中是通过正则表达式来实现的。这个概念在1970年代由Ken Thompson引入了UNIX世界。现代Python使用`re`模块来处理正则表达式。有时候`str.replace()`或者列表解析可以作为删除特定字符的简单替代方案，但它们没有正则表达式强大。实现细节方面，正则表达式引擎通常采用NFA（非确定有限自动机）或DFA（确定有限自动机）。

## See Also 参考链接
- [官方Python `re`模块文档](https://docs.python.org/3/library/re.html)
- [正则表达式快速参考](https://www.regular-expressions.info/quickstart.html)
