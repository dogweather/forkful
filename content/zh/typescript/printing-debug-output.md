---
title:                "TypeScript: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么要打印调试输出

在编写 TypeScript 代码时，打印调试输出是一个非常有用的技巧。它可以帮助开发人员检查程序的执行流程，找出错误和改进代码。通过打印调试输出，开发人员可以更快地调试代码，提高编码效率。

## 如何打印调试输出

在 TypeScript 中，可以使用 `console.log()` 来打印调试输出。例如：

```TypeScript
const num1: number = 10;
const num2: number = 20;
console.log('The sum of ' + num1 + ' and ' + num2 + ' is ' + (num1 + num2));
```

以上代码将输出：`The sum of 10 and 20 is 30`。开发人员还可以使用模板字符串来打印更复杂的调试信息。例如：

```TypeScript
const name: string = 'John';
const age: number = 25;
console.log(`Name: ${name}, Age: ${age}, Birth Year: ${new Date().getFullYear() - age}`);
```

以上代码将输出：`Name: John, Age: 25, Birth Year: 1996`。通过使用模板字符串，可以更方便地在调试信息中嵌入变量。

## 深入了解打印调试输出

除了常规的 `console.log()` 之外，TypeScript 还提供了 `console.warn()` 和 `console.error()` 来分别打印警告和错误信息。另外，开发人员还可以使用 `console.assert()` 来进行断言测试，如果断言不成立，则会打印错误信息。例如：

```typescript
const num: number = 5;
console.assert(num < 3, 'The number must be less than 3');
```

以上代码将输出：`Assertion failed: The number must be less than 3`。

## 参考链接

- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/)
- [MDN Web Docs: console](https://developer.mozilla.org/zh-CN/docs/Web/API/console)