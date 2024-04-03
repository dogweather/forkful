---
date: 2024-01-26 03:36:34.786184-07:00
description: "\u91CD\u6784\u662F\u5728\u4E0D\u6539\u53D8\u73B0\u6709\u8BA1\u7B97\u673A\
  \u4EE3\u7801\u5916\u90E8\u884C\u4E3A\u7684\u60C5\u51B5\u4E0B\uFF0C\u91CD\u65B0\u7ED3\
  \u6784\u5316\u4EE3\u7801\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u91CD\
  \u6784\uFF0C\u65E8\u5728\u4F7F\u4EE3\u7801\u66F4\u6E05\u6670\u3001\u66F4\u6613\u4E8E\
  \u7EF4\u62A4\uFF0C\u5E76\u964D\u4F4E\u590D\u6742\u6027\uFF0C\u8FD9\u6837\u5BF9\u4E8E\
  \u521D\u6B21\u63A5\u89E6\u7684\u4EBA\u6765\u8BF4\uFF0C\u4EE3\u7801\u5C31\u66F4\u5BB9\
  \u6613\u7406\u89E3\u4E86\u3002"
lastmod: '2024-03-13T22:44:47.481194-06:00'
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u662F\u5728\u4E0D\u6539\u53D8\u73B0\u6709\u8BA1\u7B97\u673A\
  \u4EE3\u7801\u5916\u90E8\u884C\u4E3A\u7684\u60C5\u51B5\u4E0B\uFF0C\u91CD\u65B0\u7ED3\
  \u6784\u5316\u4EE3\u7801\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u91CD\
  \u6784\uFF0C\u65E8\u5728\u4F7F\u4EE3\u7801\u66F4\u6E05\u6670\u3001\u66F4\u6613\u4E8E\
  \u7EF4\u62A4\uFF0C\u5E76\u964D\u4F4E\u590D\u6742\u6027\uFF0C\u8FD9\u6837\u5BF9\u4E8E\
  \u521D\u6B21\u63A5\u89E6\u7684\u4EBA\u6765\u8BF4\uFF0C\u4EE3\u7801\u5C31\u66F4\u5BB9\
  \u6613\u7406\u89E3\u4E86\u3002."
title: "\u91CD\u6784"
weight: 19
---

## 什么与为什么？
重构是在不改变现有计算机代码外部行为的情况下，重新结构化代码的过程。程序员进行重构，旨在使代码更清晰、更易于维护，并降低复杂性，这样对于初次接触的人来说，代码就更容易理解了。

## 如何进行：
考虑一个TypeScript函数，它的最佳状态已经一去不复返了——它有些乱，需要一些关爱：

```typescript
function userInfo(data: any): string {
    return "User Info: " + data.name + ", " + data.age + ", " + data.email + ";" ;
}
```
重构后，可能看起来像这样：

```typescript
interface User {
    name: string;
    age: number;
    email: string;
}

function formatUserInfo(user: User): string {
    return `User Info: ${user.name}, ${user.age}, ${user.email};`;
}
```

第二个例子利用了TypeScript的类型系统，并通过一个`接口`来避免潜在的运行时错误并提高可读性，从而更为健壮。

## 深入探讨
重构并不是一个现代概念；它随着编程的发展而演变，随着马丁·福勒的《重构：改善既有代码的设计》一书在1999年的发布而变得更加正式化。在敏捷开发环境中，重构至关重要，它促进了代码的适应性变更。一些替代手动重构的选择包括自动化工具，如TSLint 或 TypeScript自己的语言服务器，这些工具可以为您建议甚至执行某些重构任务。实施细节通常涉及识别“代码异味”，如重复代码、长方法或大型类，并应用模式来解决——如提取方法、移动到更合适的类，或使用更简单的结构。这些模式是理解重构的如何和为什么的关键。

## 另见
- [马丁·福勒的书《重构：改善既有代码的设计》](https://martinfowler.com/books/refactoring.html)
- [TSLint 用于静态代码分析](https://palantir.github.io/tslint/)
- [理解代码异味](https://refactoring.guru/refactoring/smells)
