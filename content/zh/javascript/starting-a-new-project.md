---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？ (What & Why?)

新项目的启动是创建新的编程项目或软件的过程，程序员这么做是因为他们要求解决特定的问题或实现特定的目标。

## 怎么做：(How to:)

开始一个新的JavaScript项目并不困难，下面是简单的示例：

```Javascript
mkdir new_project      // 创建一个新的目录
cd new_project         // 进入新的目录
npm init -y            // 初始化一个新的npm项目
echo "console.log('Hello World')" > index.js  // 创建一个新的JavaScript文件
node index.js          // 运行JavaScript文件
```

在上面的代码中，首先创建了一个名为 "new_project" 的新目录，并在其中初始化了一个新的npm项目。然后在这个目录中创建了一个名为 "index.js" 的JavaScript文件，并在该文件中写入一条简单的 `console.log('Hello World')` 语句。最后，使用 `node index.js` 运行这个文件。

输出：

```
Hello World
```

## 深入挖掘 (Deep Dive)

1. 历史背景：JavaScript是于1995年由NetScape发布的，用于开发Web应用程序。从那时起，它已经进行了多次更新，以改善性能并添加新的功能。

2. 替代选项：除了JavaScript，还有很多其他语言可以用来创建项目，例如Python、Java、和C#等等。

3. 实施细节：创建新项目时，一个好的实践是将代码模块化，这样就可以使代码更具可读性同时更容易进行单元测试。

## 参考资料 (See Also)

1. [MDN Web 文档 - JavaScript 教程](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Guide)
2. [W3Schools - JavaScript 教程](https://www.w3schools.com/js/)
3. [JavaScript 新手教程](https://www.runoob.com/js/js-tutorial.html)