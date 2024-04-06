---
date: 2024-01-20 18:03:54.793195-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u65B0\u5EFA\u9879\u76EE\uFF0C\
  \u9996\u5148\u5F97\u9009\u4E2A\u5DE5\u5177\u3002\u5047\u8BBE\u6211\u4EEC\u7528`Node.js`\uFF0C\
  \u4E00\u4E2A\u6D41\u884C\u7684JavaScript\u8FD0\u884C\u73AF\u5883\u3002\u5F00\u59CB\
  \u5F97\u8FD9\u6837\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.493517-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u65B0\u5EFA\u9879\u76EE\uFF0C\u9996\u5148\
  \u5F97\u9009\u4E2A\u5DE5\u5177\u3002\u5047\u8BBE\u6211\u4EEC\u7528`Node.js`\uFF0C\
  \u4E00\u4E2A\u6D41\u884C\u7684JavaScript\u8FD0\u884C\u73AF\u5883\u3002\u5F00\u59CB\
  \u5F97\u8FD9\u6837\uFF1A."
title: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE"
weight: 1
---

## How to: (如何操作：)
新建项目，首先得选个工具。假设我们用`Node.js`，一个流行的JavaScript运行环境。开始得这样：

```Javascript
// 确认Node.js已安装
// 终端（命令行）运行:
node -v

// 输出应该是Node.js的版本号，比如:
v14.15.1

// 初始化新项目
// 在项目文件夹运行:
npm init -y

// 这会创建package.json文件，它记录项目信息和依赖。
```

代码运行后，你的文件夹里会有个`package.json`，这标志着新项目的开始。

## Deep Dive (深入探究)
早年，JavaScript只能在浏览器里跑。后来Ryan Dahl发明了`Node.js`，让JavaScript能在服务器上运行。现今，还有其他选项如`Deno`，由Ryan Dahl创造，意在解决`Node.js`的不足之处。

每个新项目都得考虑代码结构，模块管理，和第三方库。JavaScript生态系统中，`npm`是管理依赖的老大，但还有`yarn`，一个更快的选择。

具体项目的话，还有许多模板和脚手架工具，比如`create-react-app`，可以快速开始一个React项目。不过，最好理解这些工具背后的原理，才能灵活使用。

## See Also (另请参阅)
- Node.js官网：[https://nodejs.org/](https://nodejs.org/)
- npm官方文档：[https://docs.npmjs.com/](https://docs.npmjs.com/)
- create-react-app官方文档：[https://create-react-app.dev/](https://create-react-app.dev/)
