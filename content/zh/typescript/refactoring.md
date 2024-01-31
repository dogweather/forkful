---
title:                "重构"
date:                  2024-01-26T03:36:34.786184-07:00
model:                 gpt-4-0125-preview
simple_title:         "重构"

category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/refactoring.md"
---

{{< edit_this_page >}}

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
