---
title:                "解析HTML"
date:                  2024-01-20T15:32:52.289581-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/parsing-html.md"
---

{{< edit_this_page >}}

# HTML解析是什么？为什么要进行？

在JavaScript中，解析HTML意味着将字符串形式的HTML代码转换成可由代码操作的DOM（文档对象模型）结构。程序员需要进行解析来动态地读取、修改、删除或插入网页内容。

# 如何操作：

以下是几个使用JavaScript对HTML字符串进行解析的示例：

```javascript
// 使用DOMParser解析HTML字符串
const parser = new DOMParser();
const htmlString = '<div id="greeting">你好, 世界!</div>';
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.firstChild); // 输出: <div id="greeting">你好, 世界!</div>

// 使用innerHTML操作HTML字符串
const div = document.createElement('div');
div.innerHTML = '<p>这是段落。</p>';
document.body.appendChild(div.firstChild); // 网页上现在有了一个新段落
```

# 深入探讨:

解析HTML的需求可以追溯到网页和JavaScript的初期。早先，当AJAX和服务端动态页面生成还未普及时，客户端HTML解析并不常见。随着Web 2.0的兴起，客户端的解析需求增加，因此浏览器开始集成像DOMParser这样的API。

除了`DOMParser`，其他一些方法比如`innerHTML`或jQuery的`$.parseHTML`等库也可用于解析HTML，但是各有利弊。`innerHTML`简单但容易受到XSS（跨站脚本）攻击；`DOMParser`更安全，但不如`innerHTML`高效。

解析性能和安全性始终是开发者考虑的重点。请务必清洗和验证外部来源的HTML，以避免安全问题。

# 另请参阅：

- MDN关于DOMParser的文档: [DOMParser](https://developer.mozilla.org/zh-CN/docs/Web/API/DOMParser)
- MDN关于innerHTML的文档: [Element.innerHTML](https://developer.mozilla.org/zh-CN/docs/Web/API/Element/innerHTML)
- 关于XSS攻击防范的文档: [防范跨站脚本 (XSS)](https://developer.mozilla.org/zh-CN/docs/Learn/Server-side/First_steps/Website_security)