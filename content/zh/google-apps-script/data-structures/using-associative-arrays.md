---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:14.702610-07:00
description: "\u5173\u8054\u6570\u7EC4\uFF0C\u5728 Google Apps \u811A\u672C\uFF08\u4E00\
  \u79CD JavaScript \u7684\u53D8\u4F53\uFF09\u4E2D\u88AB\u79F0\u4E3A\u5BF9\u8C61\uFF0C\
  \u5141\u8BB8\u7A0B\u5E8F\u5458\u521B\u5EFA\u952E\u503C\u5BF9\u7684\u96C6\u5408\u3002\
  \u8FD9\u4E00\u529F\u80FD\u5BF9\u4E8E\u5B58\u50A8\u548C\u9AD8\u6548\u64CD\u4F5C\u6570\
  \u636E\u81F3\u5173\u91CD\u8981\uFF0C\u7279\u522B\u662F\u5728\u5904\u7406\u52A8\u6001\
  \u547D\u540D\u5C5E\u6027\u6216\u5F53\u4F20\u7EDF\u6570\u7EC4\u7684\u7EBF\u6027\u5B58\
  \u50A8\u548C\u8BBF\u95EE\u6A21\u5F0F\u4E0D\u8DB3\u4EE5\u5E94\u5BF9\u65F6\u3002"
lastmod: '2024-02-25T18:49:44.822626-07:00'
model: gpt-4-0125-preview
summary: "\u5173\u8054\u6570\u7EC4\uFF0C\u5728 Google Apps \u811A\u672C\uFF08\u4E00\
  \u79CD JavaScript \u7684\u53D8\u4F53\uFF09\u4E2D\u88AB\u79F0\u4E3A\u5BF9\u8C61\uFF0C\
  \u5141\u8BB8\u7A0B\u5E8F\u5458\u521B\u5EFA\u952E\u503C\u5BF9\u7684\u96C6\u5408\u3002\
  \u8FD9\u4E00\u529F\u80FD\u5BF9\u4E8E\u5B58\u50A8\u548C\u9AD8\u6548\u64CD\u4F5C\u6570\
  \u636E\u81F3\u5173\u91CD\u8981\uFF0C\u7279\u522B\u662F\u5728\u5904\u7406\u52A8\u6001\
  \u547D\u540D\u5C5E\u6027\u6216\u5F53\u4F20\u7EDF\u6570\u7EC4\u7684\u7EBF\u6027\u5B58\
  \u50A8\u548C\u8BBF\u95EE\u6A21\u5F0F\u4E0D\u8DB3\u4EE5\u5E94\u5BF9\u65F6\u3002"
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
---

{{< edit_this_page >}}

## 什么 & 为什么？

关联数组，在 Google Apps 脚本（一种 JavaScript 的变体）中被称为对象，允许程序员创建键值对的集合。这一功能对于存储和高效操作数据至关重要，特别是在处理动态命名属性或当传统数组的线性存储和访问模式不足以应对时。

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
