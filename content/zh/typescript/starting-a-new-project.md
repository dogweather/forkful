---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

**启动新项目**其实就是创建和设置一款全新的软件产品。这是程序员常常进行的工作，主要是为了开发具备新功能，或者满足特定需求的软件。

## 如何进行：

新建一个TypeScript项目其实非常简单, 只需要几个步骤：

首先, 确保已经安装了node.js环境，然后，执行以下命令安装TypeScript和ts-node:

```bash
npm install -g typescript ts-node
```

然后，创建一个新的目录，并在该目录中初始化npm：
```bash
mkdir my_project && cd my_project
npm init -y
```

接着，创建一个tsconfig.json文件，并添加以下设置：
```bash
touch tsconfig.json
```

```json
{
  "compilerOptions": {
    "module": "commonjs",
    "target": "es6",
    "outDir": "dist",
    "rootDir": "src",
    "strict": true
  }
}
```
最后，新建一个'src'目录，在其中添加你的TypeScript代码。

## 深入探讨：

- **历史背景**：程序员创建新项目的趋势，可以追溯到计算机软件开始流行的时候。 对于一些程序员来说，创建新项目也是他们持续学习新技术，解决新需求的一个重要过程。

- **实现细节**：在TypeScript中，可以使用各种工具，例如Webpack或者Parcel来帮助你设置和构建项目。TypeScript的优点在于它是JavaScript的超集，它引入了类型和其他一些高级特性，使得JavaScript的开发更为可控和高效。

- **可取代方式**：实际上，有很多其它的方式可以创建新的编程项目，比如直接使用JavaScript，或者使用Python，Java等不同的编程语言。不过，TypeScript由于其对JavaScript的兼容性和扩展性，以及强大的类型系统，被很多现代前端项目所选择。

## 查看更多：

- [TypeScript官方文档](https://www.typescriptlang.org/)
- [TypeScript Github项目](https://github.com/microsoft/TypeScript)
- [深入了解TypeScript](https://www.udemy.com/course/typescript-the-complete-developers-guide/)