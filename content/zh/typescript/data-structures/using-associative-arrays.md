---
aliases:
- /zh/typescript/using-associative-arrays/
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:22.877086-07:00
description: "\u5728TypeScript\u4E2D\uFF0C\u5173\u8054\u6570\u7EC4\u6216\u5BF9\u8C61\
  \u5141\u8BB8\u4F60\u4F7F\u7528\u5B57\u7B26\u4E32\uFF08\u6216\u952E\uFF09\u6765\u8BBF\
  \u95EE\u503C\u5BF9\u3002\u4E0E\u4F20\u7EDF\u6570\u7EC4\u76F8\u6BD4\uFF0C\u7A0B\u5E8F\
  \u5458\u4F7F\u7528\u5B83\u4EEC\u6765\u5B9E\u73B0\u66F4\u52A8\u6001\u7684\u6570\u636E\
  \u8BBF\u95EE\u6A21\u5F0F\uFF0C\u63D0\u4F9B\u4E86\u4E00\u79CD\u7075\u6D3B\u7684\u65B9\
  \u5F0F\u6765\u6784\u5EFA\u548C\u8BBF\u95EE\u6570\u636E\uFF0C\u800C\u4E0D\u7528\u4F9D\
  \u8D56\u4E8E\u6570\u5B57\u7D22\u5F15\u3002"
lastmod: 2024-02-18 23:08:58.897642
model: gpt-4-0125-preview
summary: "\u5728TypeScript\u4E2D\uFF0C\u5173\u8054\u6570\u7EC4\u6216\u5BF9\u8C61\u5141\
  \u8BB8\u4F60\u4F7F\u7528\u5B57\u7B26\u4E32\uFF08\u6216\u952E\uFF09\u6765\u8BBF\u95EE\
  \u503C\u5BF9\u3002\u4E0E\u4F20\u7EDF\u6570\u7EC4\u76F8\u6BD4\uFF0C\u7A0B\u5E8F\u5458\
  \u4F7F\u7528\u5B83\u4EEC\u6765\u5B9E\u73B0\u66F4\u52A8\u6001\u7684\u6570\u636E\u8BBF\
  \u95EE\u6A21\u5F0F\uFF0C\u63D0\u4F9B\u4E86\u4E00\u79CD\u7075\u6D3B\u7684\u65B9\u5F0F\
  \u6765\u6784\u5EFA\u548C\u8BBF\u95EE\u6570\u636E\uFF0C\u800C\u4E0D\u7528\u4F9D\u8D56\
  \u4E8E\u6570\u5B57\u7D22\u5F15\u3002"
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
---

{{< edit_this_page >}}

## 什么 & 为什么?

在TypeScript中，关联数组或对象允许你使用字符串（或键）来访问值对。与传统数组相比，程序员使用它们来实现更动态的数据访问模式，提供了一种灵活的方式来构建和访问数据，而不用依赖于数字索引。

## 如何操作:

在TypeScript中创建和使用关联数组非常直接。这里有一个基本的操作流程：

```TypeScript
// 声明一个关联数组
let user: { [key: string]: string } = {};

// 添加数据
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

输出：

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

遍历键值对也很简单：

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

输出：

```TypeScript
name: Jane Doe
email: jane@example.com
```

如果你正在处理混合数据类型，TypeScript的类型系统将派上用场：

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

输出：

```TypeScript
{ name: 'John Doe', age: 30 }
```

## 深入了解

在TypeScript中，我们所说的关联数组本质上是对象。历史上，在诸如PHP之类的语言中，关联数组是一种基本类型，但JavaScript（以及TypeScript）使用对象来实现这一目标。这种方法既是一种优势也是一种局限。对象为将字符串与值关联提供了高度动态的结构，但它们并不是用来作为传统意义上的'数组'。例如，你不能直接在这些对象上使用数组方法，如`push`或`pop`。

对于需要有序的键值对集合且具有类数组操作的情况，TypeScript（和现代JavaScript）提供了`Map`对象：

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

虽然TypeScript的类型系统和ES6特性如`Map`提供了强大的替代方案，但理解如何将对象用作关联数组在处理对象字面量更高效或者处理JSON数据结构的场景下是有用的。关键是选择适合任务的正确工具。
