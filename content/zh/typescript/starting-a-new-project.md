---
title:                "开始一个新的项目"
html_title:           "TypeScript: 开始一个新的项目"
simple_title:         "开始一个新的项目"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 新项目是什么？为什么要开始一个新项目？
开始一个新项目可以理解为使用编程语言创建一个新的软件项目。程序员们会经常这样做是因为需要实现新的功能、改进旧的项目或者应对新的市场需求。

## 如何开始一个新项目：
```TypeScript
//创建一个新的TypeScript文件
touch new_project.ts

//声明一个类型为字符串的变量
let message: string = '欢迎来到新的项目！';

//打印变量的值
console.log(message);

//编译TypeScript文件
tsc new_project.ts

//执行编译后生成的JavaScript文件
node new_project.js

//输出：欢迎来到新的项目！
```

## 深入了解：
创建新项目的方式因编程语言而异。在TypeScript中，我们可以使用 ```tsc``` 命令行工具来编译代码，并且最终将会生成JavaScript文件。如果你不想手动编译每次更改，你也可以使用 TypeScript的自动编译功能。除了TypeScript，其他流行的编程语言也有类似的工具来帮助开发者更便捷地创建新的项目。

## 参考资料：
- [TypeScript官方文档](https://www.typescriptlang.org/docs/handbook/gulp.html)
- [其他流行编程语言的项目创建方式](https://www.linuxtopia.org/online_books/programming_books/art_of_unix_programming/ch12.html)