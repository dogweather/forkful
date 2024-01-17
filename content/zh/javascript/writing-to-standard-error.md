---
title:                "写入标准错误"
html_title:           "Javascript: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 什么是标准错误并为什么程序员要使用它？

标准错误是一个程序在运行过程中产生的错误信息的输出流。它通常用于调试和错误处理，可以帮助程序员快速定位并修复程序中的错误。程序员使用标准错误来获得有用的反馈，而不是程序中断或崩溃。

## 如何使用标准错误：

### 打印到标准错误：

```Javascript
console.error("这是一个错误信息");
```

输出: "这是一个错误信息" (红色文本)

### 抛出错误到标准错误：

```Javascript
throw new Error("这是一个错误信息");
```

输出: "Error: 这是一个错误信息" (红色文本)

## 深入了解：

### 历史背景：

标准错误的概念源自Unix系统，在20世纪70年代首次被引入。它是一种标准输出流的扩展，用于区分普通输出和错误输出。

### 替代方法：

除了使用标准错误打印错误信息，程序员也可以使用其他方式，比如使用try/catch语句来捕捉错误或使用日志记录器来记录错误信息。然而，标准错误仍然被广泛使用，因为它简单直接，不需要额外的依赖。

### 实现细节：

在Javascript中，标准错误是由console.error()和throw语句来实现的。它们都会将错误信息输出到标准错误流中，由系统负责处理。

## 参考文章：

- ["Understanding Node.js' Console Object"](https://developer.ibm.com/zh/languages/nodejs/articles/understanding-nodejs-console-object/)
- ["What is Standard Error?"](https://www.geeksforgeeks.org/what-is-standard-error/)
- ["The Difference Between Standard Output and Standard Error"](https://www.lifewire.com/standard-output-vs-standard-error-2626112)