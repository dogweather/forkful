---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:19.559636-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Fish shell\u4E3B\u8981\u4E0D\u662F\u4E3A\
  \u76F4\u63A5\u89E3\u6790HTML\u800C\u8BBE\u8BA1\u7684\u3002\u7136\u800C\uFF0C\u5B83\
  \u64C5\u957F\u5C06Unix\u5DE5\u5177\u5982`curl`\u3001`grep`\u3001`sed`\u3001`awk`\uFF0C\
  \u6216\u4F7F\u7528\u4E13\u95E8\u5DE5\u5177\u5982`pup`\u6216\u5728Python\u811A\u672C\
  \u4E2D\u7684`beautifulsoup`\u7ED3\u5408\u8D77\u6765\u3002\u4E0B\u9762\u7684\u4F8B\
  \u5B50\u5C55\u793A\u4E86\u5982\u4F55\u5728Fish\u2026"
lastmod: '2024-03-13T22:44:48.263240-06:00'
model: gpt-4-0125-preview
summary: "Fish shell\u4E3B\u8981\u4E0D\u662F\u4E3A\u76F4\u63A5\u89E3\u6790HTML\u800C\
  \u8BBE\u8BA1\u7684\u3002\u7136\u800C\uFF0C\u5B83\u64C5\u957F\u5C06Unix\u5DE5\u5177\
  \u5982`curl`\u3001`grep`\u3001`sed`\u3001`awk`\uFF0C\u6216\u4F7F\u7528\u4E13\u95E8\
  \u5DE5\u5177\u5982`pup`\u6216\u5728Python\u811A\u672C\u4E2D\u7684`beautifulsoup`\u7ED3\
  \u5408\u8D77\u6765\u3002\u4E0B\u9762\u7684\u4F8B\u5B50\u5C55\u793A\u4E86\u5982\u4F55\
  \u5728Fish shell\u4E2D\u5229\u7528\u8FD9\u4E9B\u5DE5\u5177\u6765\u89E3\u6790HTML."
title: "\u89E3\u6790HTML"
weight: 43
---

## 如何操作：
Fish shell主要不是为直接解析HTML而设计的。然而，它擅长将Unix工具如`curl`、`grep`、`sed`、`awk`，或使用专门工具如`pup`或在Python脚本中的`beautifulsoup`结合起来。下面的例子展示了如何在Fish shell中利用这些工具来解析HTML。

### 使用`curl`和`grep`：
获取HTML内容并提取包含链接的行：

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

输出：
```
/page1.html
/page2.html
...
```

### 使用`pup`（一个用于解析HTML的命令行工具）：
首先，确保安装了`pup`。然后你可以使用它按标签、id、类等提取元素。

```fish
curl -s https://example.com | pup 'a attr{href}'
```

输出，类似于`grep`例子，会列出`<a>`标签的href属性。

### 使用Python脚本和`beautifulsoup`：
虽然Fish本身不能直接解析HTML，但它可以无缝集成Python脚本。下面是一个使用Python和`BeautifulSoup`解析并提取HTML标题的简洁示例。确保你的Python环境中安装了`beautifulsoup4`和`requests`。

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

response = requests.get(sys.argv[1])
soup = BeautifulSoup(response.text, 'html.parser')

titles = soup.find_all('title')

for title in titles:
    print(title.get_text())
" $url
end
```

用法：

```fish
parse_html 'https://example.com'
```

输出：
```
Example Domain
```

这些方法各适应不同的用例和复杂性程度，从简单的命令行文本操作到在Python脚本中使用`beautifulsoup`的完整解析能力。根据你的需求和HTML结构的复杂性，你可能选择直接的Unix管道或更强大的脚本处理方式。
