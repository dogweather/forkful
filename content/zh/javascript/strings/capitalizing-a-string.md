---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:41.800618-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728JavaScript\u4E2D\uFF0C\u6CA1\u6709\
  \u4E00\u4E2A\u5185\u5EFA\u65B9\u6CD5\u53EF\u4EE5\u76F4\u63A5\u5B9E\u73B0\u5B57\u7B26\
  \u4E32\u9996\u5B57\u6BCD\u5927\u5199\uFF0C\u4F46\u4F7F\u7528\u57FA\u672C\u7684\u5B57\
  \u7B26\u4E32\u64CD\u4F5C\u65B9\u6CD5\u6765\u5B9E\u73B0\u8FD9\u4E00\u70B9\u5F88\u7B80\
  \u5355\u3002 #."
lastmod: '2024-03-13T22:44:48.186951-06:00'
model: gpt-4-0125-preview
summary: "\u5728JavaScript\u4E2D\uFF0C\u6CA1\u6709\u4E00\u4E2A\u5185\u5EFA\u65B9\u6CD5\
  \u53EF\u4EE5\u76F4\u63A5\u5B9E\u73B0\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\
  \uFF0C\u4F46\u4F7F\u7528\u57FA\u672C\u7684\u5B57\u7B26\u4E32\u64CD\u4F5C\u65B9\u6CD5\
  \u6765\u5B9E\u73B0\u8FD9\u4E00\u70B9\u5F88\u7B80\u5355."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 如何操作：
在JavaScript中，没有一个内建方法可以直接实现字符串首字母大写，但使用基本的字符串操作方法来实现这一点很简单。

### 使用标准JavaScript
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // 输出："Hello world"
```

### ES6版本
利用ES6模板字符串，可以更简洁地编写该函数：
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // 输出："Hello ES6"
```

### 使用Lodash
Lodash是一个流行的第三方实用程序库，提供了广泛的功能来操作和处理JavaScript值，包括字符串。使用Lodash来实现字符串首字母大写：
```javascript
// 首先，如果你还没有安装lodash：npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH example')); // 输出："Lodash example"
```
_注意，Lodash不仅将首字母大写，还将字符串的其余部分转换为小写，这与纯JavaScript实现略有不同。_

### 使用CSS（仅用于显示目的）
如果目标是为了在UI中显示首字母大写的文本，可以使用CSS：
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- 显示为 "Hello css" -->
```
**注意：**这种方法改变了文本在网页上的显示方式，而不是在JavaScript中更改字符串本身。
