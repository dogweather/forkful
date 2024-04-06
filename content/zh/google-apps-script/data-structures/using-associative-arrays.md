---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:14.702610-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Google Apps \u811A\u672C\u4E2D\
  \uFF0C\u4F7F\u7528\u5927\u62EC\u53F7 `{}` \u521B\u5EFA\u548C\u64CD\u4F5C\u5173\u8054\
  \u6570\u7EC4\uFF08\u5BF9\u8C61\uFF09\uFF0C\u5728\u5176\u4E2D\u5B9A\u4E49\u952E\u503C\
  \u5BF9\u3002\u952E\u662F\u552F\u4E00\u6807\u8BC6\u7B26\uFF0C\u503C\u53EF\u4EE5\u662F\
  \u4ECE\u5B57\u7B26\u4E32\u548C\u6570\u5B57\u5230\u66F4\u590D\u6742\u7684\u5BF9\u8C61\
  \u6216\u51FD\u6570\u7684\u4EFB\u4F55\u4E1C\u897F\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\
  \u57FA\u672C\u7684\u4F8B\u5B50\uFF1A."
lastmod: '2024-04-05T21:53:47.548606-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

## 如何操作：
在 Google Apps 脚本中，使用大括号 `{}` 创建和操作关联数组（对象），在其中定义键值对。键是唯一标识符，值可以是从字符串和数字到更复杂的对象或函数的任何东西。这里有一个基本的例子：

```javascript
function createAssociativeArray() {
  var user = {
    name: "John Doe",
    age: 30,
    email: "johndoe@example.com"
  };

  // 访问值
  Logger.log(user.name); // 输出：John Doe
  Logger.log(user["email"]); // 输出：johndoe@example.com

  // 添加新的键值对
  user.title = "软件开发者";
  user["country"] = "USA";

  Logger.log(user.title); // 输出：软件开发者

  // 迭代键值对
  for (var key in user) {
    Logger.log(key + ': ' + user[key]);
  }
}
```

对迭代部分的样本输出可能看起来像这样：
```
name: John Doe
age: 30
email: johndoe@example.com
title: 软件开发者
country: USA
```

注意你可以使用点表示法和括号表示法来访问和设置属性。当处理动态确定的键或包括在标识符中不允许的字符时，括号表示法特别有用。

## 深入了解
作为对象的关联数组一直是 JavaScript，以及通过扩展的 Google Apps 脚本，反映其基于原型的继承机制的基石。与传统关联数组或字典（例如，Python 的 dict）的语言不同，Google Apps 脚本对象提供了一种灵活而强大的数据结构方式，受益于 JavaScript 的动态特性。

然而，值得注意的是，ECMAScript 2015 规范引入了 `Map` 和 `Set` 对象，提供了一种更直接的关联集合处理方式，与对象相比，它们在保持插入顺序和处理大型数据集方面性能更好。尽管 Google Apps 脚本也支持这些，但在使用对象或较新的 `Map`/`Set` 结构之间的选择取决于特定的需求和性能考虑。对于大多数关联数组任务，传统的基于对象的实现提供了一个熟悉且多功能的方法，但随着脚本复杂性的增加，检查较新的替代方案是可取的。
