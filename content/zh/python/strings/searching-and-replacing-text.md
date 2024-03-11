---
date: 2024-01-20 17:58:24.719137-07:00
description: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u5C31\u662F\u627E\u7279\u5B9A\
  \u5B57\u7B26\u6216\u5B57\u7B26\u4E32\uFF0C\u7136\u540E\u7528\u5176\u4ED6\u7684\u5185\
  \u5BB9\u66FF\u6362\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u6279\
  \u91CF\u4FEE\u6539\u4EE3\u7801\u6216\u6570\u636E\uFF0C\u63D0\u9AD8\u6548\u7387\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.003621-06:00'
model: gpt-4-1106-preview
summary: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u5C31\u662F\u627E\u7279\u5B9A\
  \u5B57\u7B26\u6216\u5B57\u7B26\u4E32\uFF0C\u7136\u540E\u7528\u5176\u4ED6\u7684\u5185\
  \u5BB9\u66FF\u6362\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u6279\
  \u91CF\u4FEE\u6539\u4EE3\u7801\u6216\u6570\u636E\uFF0C\u63D0\u9AD8\u6548\u7387\u3002"
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
---

{{< edit_this_page >}}

## What & Why? (是什么和为什么？)
搜索和替换文本就是找特定字符或字符串，然后用其他的内容替换。程序员这么做是为了批量修改代码或数据，提高效率。

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
