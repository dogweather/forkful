---
title:                "搜索和替换文本"
aliases: - /zh/python/searching-and-replacing-text.md
date:                  2024-01-20T17:58:24.719137-07:00
model:                 gpt-4-1106-preview
simple_title:         "搜索和替换文本"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/searching-and-replacing-text.md"
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
