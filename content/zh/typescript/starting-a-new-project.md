---
title:                "TypeScript: 开始一个新项目。"
simple_title:         "开始一个新项目。"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

# 为什么要开始一个新的项目？

开始一个新的项目可能是因为有新的想法，或者想要提升技术水平。无论是什么原因，开始一个新的项目都是很有意义的。

## 如何开始一个新的项目？

首先，在你的电脑上安装TypeScript。然后，根据你的项目需求，选择合适的编辑器，如Visual Studio Code或WebStorm。

接下来，创建一个新的TypeScript项目，并在终端窗口中转到项目目录。运行以下命令来初始化一个新的TypeScript项目：

```TypeScript
npm init
```
这条命令将创建一个package.json文件，其中包含项目的相关信息。然后使用下面的命令安装TypeScript：

```TypeScript
npm install typescript --save-dev
```
一旦安装完成，就可以开始编写代码了。使用以下命令来创建一个名为“index.ts”的TypeScript文件：

```TypeScript
touch index.ts
```
在该文件中，可以使用以下代码来输出一句“Hello World！”：

```TypeScript
console.log("Hello World!");
```
在终端窗口中，使用以下命令来编译TypeScript代码：

```TypeScript
tsc index.ts
```
这将产生一个名为“index.js”的JavaScript文件，它将包含与TypeScript代码相同的代码。最后，可以使用以下命令来运行JavaScript文件：

```TypeScript
node index.js
```
在终端窗口中，你将看到“Hello World！”这句话被输出。

## 深入了解开始一个新的项目

在开始一个新的TypeScript项目时，可以选择使用一些有用的工具和框架来提升效率和质量。比如，可以使用Angular框架来构建复杂的Web应用程序，或者使用Express框架来构建服务端应用程序。同时，TypeScript也支持一些常用的第三方库，如Lodash和Moment等。

另外，为了保持代码的可读性和可维护性，建议在项目中添加注释，并按照一定的编码规范来编写代码。

# 请参阅

- [TypeScript官方文档](https://www.typescriptlang.org/docs/home.html)
- [Angular](https://angular.io/)
- [Express](https://expressjs.com/)
- [Lodash](https://lodash.com/)
- [Moment](https://momentjs.com/)