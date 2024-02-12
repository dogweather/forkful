---
title:                "开始一个新项目"
aliases: - /zh/javascript/starting-a-new-project.md
date:                  2024-01-20T18:03:54.793195-07:00
model:                 gpt-4-1106-preview
simple_title:         "开始一个新项目"

tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
新项目就是从零开始的代码创作。程序员这么做为了解决问题，实现想法，或者学习新技术。

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
