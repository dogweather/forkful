---
date: 2024-01-20 17:58:24.719137-07:00
description: "How to (\u5982\u4F55\u64CD\u4F5C) Python\u91CC\u641C\u7D22\u548C\u66FF\
  \u6362\u7684\u5F3A\u5927\u5DE5\u5177\u662F`str.replace()`\u65B9\u6CD5\u548C`re.sub()`\u51FD\
  \u6570\u3002\u4E0B\u9762\u662F\u7B80\u5355\u793A\u4F8B\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.420252-06:00'
model: gpt-4-1106-preview
summary: "How to (\u5982\u4F55\u64CD\u4F5C) Python\u91CC\u641C\u7D22\u548C\u66FF\u6362\
  \u7684\u5F3A\u5927\u5DE5\u5177\u662F`str.replace()`\u65B9\u6CD5\u548C`re.sub()`\u51FD\
  \u6570\u3002\u4E0B\u9762\u662F\u7B80\u5355\u793A\u4F8B\uFF1A."
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
weight: 10
---

## How to (如何操作)
Python里搜索和替换的强大工具是`str.replace()`方法和`re.sub()`函数。下面是简单示例：

```python
# 使用 str.replace()
original_text = "Hello World! World is beautiful."
new_text = original_text.replace("World", "Universe")
print(new_text)
```

输出：
```plaintext
Hello Universe! Universe is beautiful.
```

正则表达式版本：

```python
import re

# 使用 re.sub()
original_text = "Hello World! World is beautiful. World 2023."
new_text = re.sub(r"World (\d+)", r"Universe \1", original_text)
print(new_text)
```

输出：
```plaintext
Hello World! World is beautiful. Universe 2023.
```

## Deep Dive (深入挖掘)
搜索和替换功能可以追溯到早期文字处理软件。比如，`sed`命令在Unix系统上就能用来搜索和替换文本。`str.replace()`理想于简单替换，没捉住细节。`re.sub()`提供了更高的灵活性，可以用正则表达式定义复杂的搜索模式。

现代文本编辑器和IDE内置了搜索和替换功能，支持基础到复杂匹配。Python中的`str.replace()`不能处理正则表达式，所以复杂情况下咱们得用`re.sub()`。

还有很多第三方库，比如`regex`，提供更加强大的搜索替换功能。

## See Also (另请参阅)
- [Python `str.replace()` 方法官方文档](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Python `re` 模块官方文档](https://docs.python.org/3/library/re.html)
- [维基百科：正则表达式](https://zh.wikipedia.org/wiki/正则表达式)
