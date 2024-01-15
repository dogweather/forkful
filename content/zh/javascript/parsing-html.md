---
title:                "解析 HTML"
html_title:           "Javascript: 解析 HTML"
simple_title:         "解析 HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/parsing-html.md"
---

{{< edit_this_page >}}

# 为什么

为什么会有人选择解析HTML呢？怎么说呢，解析HTML可以让我们更方便地从网页中提取出想要的信息，从而让我们能够更有效地处理数据。

# 怎么做

在Javascript中，我们可以使用内置的`DOMParser`对象来解析HTML。下面是一个简单的例子，我们将解析一个简单的HTML代码，并输出其中的h1标签的内容:

```javascript
const htmlString = "<h1>Hello, world!</h1>"; // 假设这里是我们从网页上获取的HTML代码
const parser = new DOMParser(); // 创建DOMParser对象
const htmlDocument = parser.parseFromString(htmlString, "text/html"); // 使用parseFromString方法解析并生成一个HTML文档对象
const h1 = htmlDocument.querySelector("h1"); // 使用querySelector方法获取h1标签
console.log(h1.textContent); // 输出"h1"标签的文本内容，这里会显示"Hello, world!"
```

# 深入了解

解析HTML其实是一个相对复杂的过程。当我们调用`DOMParser`的`parseFromString`方法时，它实际上会将HTML代码转换成一个完整的HTML文档对象，包括DOM树结构、CSS样式和Javascript代码。因此，我们可以使用一系列的DOM操作方法来从中提取我们需要的内容，比如`querySelector`、`getElementsByTagName`等等。此外，我们还可以使用`innerHTML`属性来直接获取某个元素的内部HTML代码。

# 参考资料

- [MDN - DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- [W3Schools - DOM Nodes](https://www.w3schools.com/js/js_htmldom_nodes.asp)
- [MDN - Document Object Model (DOM)](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model)