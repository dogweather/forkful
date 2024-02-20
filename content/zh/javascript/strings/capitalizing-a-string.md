---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:41.800618-07:00
description: "\u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\u6307\u7684\u662F\
  \u5C06\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\u4E2A\u5B57\u7B26\u8F6C\u6362\u4E3A\u5927\
  \u5199\uFF0C\u540C\u65F6\u4FDD\u6301\u5176\u4F59\u5B57\u7B26\u4E0D\u53D8\u3002\u8FD9\
  \u79CD\u64CD\u4F5C\u5728JavaScript\u4E2D\u5E38\u5E38\u7528\u4E8E\u683C\u5F0F\u5316\
  \u7528\u6237\u8F93\u5165\u3001\u663E\u793A\u540D\u5B57\u6216\u6807\u9898\uFF0C\u4EE5\
  \u53CA\u786E\u4FDD\u7528\u6237\u754C\u9762\u6587\u672C\u7684\u4E00\u81F4\u6027\u3002"
lastmod: 2024-02-19 22:05:07.247712
model: gpt-4-0125-preview
summary: "\u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\u6307\u7684\u662F\
  \u5C06\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\u4E2A\u5B57\u7B26\u8F6C\u6362\u4E3A\u5927\
  \u5199\uFF0C\u540C\u65F6\u4FDD\u6301\u5176\u4F59\u5B57\u7B26\u4E0D\u53D8\u3002\u8FD9\
  \u79CD\u64CD\u4F5C\u5728JavaScript\u4E2D\u5E38\u5E38\u7528\u4E8E\u683C\u5F0F\u5316\
  \u7528\u6237\u8F93\u5165\u3001\u663E\u793A\u540D\u5B57\u6216\u6807\u9898\uFF0C\u4EE5\
  \u53CA\u786E\u4FDD\u7528\u6237\u754C\u9762\u6587\u672C\u7684\u4E00\u81F4\u6027\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
---

{{< edit_this_page >}}

## 什么 & 为什么？
将字符串首字母大写指的是将字符串的第一个字符转换为大写，同时保持其余字符不变。这种操作在JavaScript中常常用于格式化用户输入、显示名字或标题，以及确保用户界面文本的一致性。

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
