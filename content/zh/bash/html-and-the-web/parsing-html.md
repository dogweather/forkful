---
title:                "解析HTML"
aliases:
- /zh/bash/parsing-html.md
date:                  2024-02-03T19:11:28.325215-07:00
model:                 gpt-4-0125-preview
simple_title:         "解析HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

解析 HTML 意味着仔细检查 HTML 文件的结构和内容，以提取信息。程序员这样做是为了访问数据、操作内容或抓取网站。

## 如何操作：

Bash 不是解析 HTML 的首选工具，但可以使用像 `grep`、`awk`、`sed` 或外部工具如 `lynx`。为了增强鲁棒性，我们将使用 `libxml2` 包中的 `xmllint`。

```bash
# 如有必要，安装 xmllint
sudo apt-get install libxml2-utils

# 示例 HTML
cat > sample.html <<EOF
<html>
<head>
  <title>示例页面</title>
</head>
<body>
  <h1>你好，Bash!</h1>
  <p id="myPara">Bash 可以读取我。</p>
</body>
</html>
EOF

# 解析标题
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "标题是：$title"

# 通过 ID 提取段落
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "段落内容是：$para"
```

输出：
```
标题是：示例页面
段落内容是：Bash 可以读取我。
```

## 深入探讨

在过去，程序员使用基于正则表达式的工具，如 `grep` 来扫描 HTML，但这很笨拙。HTML 不是规则的——它是上下文的。传统工具错过了这一点，可能会出错。

有替代方案吗？很多。Python 与 Beautiful Soup、PHP 与 DOMDocument、JavaScript 与 DOM 解析器——设计有理解 HTML 结构的库的语言。

在 bash 脚本中使用 `xmllint` 对于简单任务是可靠的。它理解 XML，以及通过扩展，XHTML。然而，常规 HTML 可能是不可预测的。它不总是遵循 XML 的严格规则。`xmllint` 将 HTML 强制进入 XML 模型，这对于格式良好的 HTML 很有效，但可能会在杂乱的内容上遇到问题。

## 另请参阅

- [W3Schools - HTML DOM 解析器](https://www.w3schools.com/xml/dom_intro.asp)：解密 HTML DOM。
- [MDN Web Docs - 解析和序列化 XML](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML)：适用于 XHTML 的 XML 解析原则。
- [Beautiful Soup 文档](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)：一个用于 HTML 解析的 Python 库。
- [libxml2 文档](http://xmlsoft.org/)：关于 `xmllint` 及相关 XML 工具的详情。
