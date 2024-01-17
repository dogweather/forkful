---
title:                "标准错误的编写"
html_title:           "TypeScript: 标准错误的编写"
simple_title:         "标准错误的编写"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 是什么 & 为什么？
写入标准错误是一种编程技术，用于在程序运行时输出错误信息。程序员使用它来调试和修复错误，以便程序可以更准确地运行。

# 如何：
```TypeScript
console.error("ERROR!"); //输出错误信息到标准错误
```

在控制台中，你会看到类似于以下的输出：
```
ERROR!
```

# 深入探讨：
1. **历史背景：** 写入标准错误的概念首先由Unix操作系统引入，它可以帮助开发者识别和解决程序运行时的错误。
2. **替代方法：** 除了使用console.error()函数，程序员也可以使用console.log()来输出错误信息。但是，console.error()输出的内容会被区分为错误级别，使其更易于识别。
3. **实现细节：** 在TypeScript中，console.error()函数由控制台对象提供，该对象提供了各种用于输出信息的函数。

# 查看相关资源：
- [TypeScript控制台对象](https://www.typescriptlang.org/docs/handbook/console.html)
- [如何使用控制台输出错误信息](https://www.tutorialspoint.com/typescript/typescript_error_handling.htm)