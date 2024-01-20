---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

打印调试输出，就是让程序将重要信息输出到控制台上。程序员这么做主要是为了跟踪程序执行过程，更轻松地查找与修复bug。

## 怎么做：

假设我们有个简单的程序，希望它能把輸出信息打印到控制台上。TypeScript提供了一个内置的console对象来完成这个任务。

```TypeScript
let num: number = 5;

// 打印数字
console.log(num);

let str: string = "Hello, TypeScript";

// 打印字符串
console.log(str);
```

执行这段代码，输出如下：

```TypeScript
5
Hello, TypeScript
```

## 深度剖析：

- 历史背景：一开始，程序员在调试他们的程序时，没有“控制台”，所以经常用注释或者临时休息的方法来显示重要信息的状态。但这种方法效率很低，所以控制台的概念就诞生了。

- 方案替代：虽然最常见的方法是使用console.log进行打印，但我们也可以使用其他一些工具比如debuggers，它可以提供更深度的调试。

- 实现细节：在 TypeScript 中，console.log() 可以接受任何类型作为输入，并且可以接受多个输入参数。这些输入参数将按照它们在参数列表中的顺序打印出来。

```TypeScript
let num: number = 5;
let str: string = "Hello, TypeScript";

// 打印多个输入参数
console.log(num, str);
```

输出如下：

```TypeScript
5 "Hello, TypeScript"
```

## 参考资料：

- [MDN 文档](https://developer.mozilla.org/zh-CN/docs/Web/API/Console/log)，对 console 对象的详细介绍。
- [TypeScript 官方文档](https://www.typescriptlang.org/docs)，提供了打印和调试功能的深度解析。