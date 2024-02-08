---
title:                "使用关联数组"
aliases:
- zh/google-apps-script/using-associative-arrays.md
date:                  2024-02-01T22:04:14.702610-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用关联数组"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/google-apps-script/using-associative-arrays.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
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
