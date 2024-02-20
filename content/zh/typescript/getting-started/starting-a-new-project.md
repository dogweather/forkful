---
date: 2024-01-20 18:04:38.783323-07:00
description: "\u5F00\u59CB\u65B0\u9879\u76EE\u5C31\u662F\u521B\u5EFA\u4E00\u4E2A\u5168\
  \u65B0\u7684\u7F16\u7A0B\u73AF\u5883\uFF0C\u4E3A\u5F00\u53D1\u8F6F\u4EF6\u6253\u4E0B\
  \u57FA\u7840\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u89E3\u51B3\
  \u65B0\u95EE\u9898\uFF0C\u5C1D\u8BD5\u521B\u610F\uFF0C\u6216\u8005\u54CD\u5E94\u5E02\
  \u573A\u9700\u6C42\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.488108
model: gpt-4-1106-preview
summary: "\u5F00\u59CB\u65B0\u9879\u76EE\u5C31\u662F\u521B\u5EFA\u4E00\u4E2A\u5168\
  \u65B0\u7684\u7F16\u7A0B\u73AF\u5883\uFF0C\u4E3A\u5F00\u53D1\u8F6F\u4EF6\u6253\u4E0B\
  \u57FA\u7840\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u89E3\u51B3\
  \u65B0\u95EE\u9898\uFF0C\u5C1D\u8BD5\u521B\u610F\uFF0C\u6216\u8005\u54CD\u5E94\u5E02\
  \u573A\u9700\u6C42\u3002"
title: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
开始新项目就是创建一个全新的编程环境，为开发软件打下基础。程序员这么做是为了解决新问题，尝试创意，或者响应市场需求。

## How to: (如何操作：)
创建新的TypeScript项目非常直接。安装Node.js和npm后，按照以下步骤：

```typescript
// 安装TypeScript全局命令
npm install -g typescript

// 创建新项目文件夹
mkdir my-new-project
cd my-new-project

// 初始化npm并创建package.json
npm init -y

// 安装TypeScript作为开发依赖
npm install --save-dev typescript

// 生成tsconfig.json配置文件
npx tsc --init

// 创建一个简单的TypeScript文件
echo 'console.log("Hello, TypeScript!");' > hello.ts

// 编译并运行
npx tsc hello.ts
node hello.js
```

输出应为：

```
Hello, TypeScript!
```

## Deep Dive (深入了解)
TypeScript诞生于2012年，旨在为JavaScript增加类型系统，提高大型项目的可维护性。新项目可以使用TypeScript CLI或现代前端框架(如Angular, Vue,或React)的脚手架工具来快速启动。例如，React项目可使用`create-react-app`模版；Angular有`ng new`命令。每种方法都提供了不同层次的自由度和预设配置。

## See Also (另请参阅)
- TypeScript官方文档：[TypeScript Language](https://www.typescriptlang.org/docs/)
- Node.js官方网站：[Node.js](https://nodejs.org/)
- npm官方文档：[npm Docs](https://docs.npmjs.com/)
- `create-react-app`文档：[Getting Started with Create React App](https://create-react-app.dev/docs/getting-started/)
- Angular CLI官方网站：[Angular CLI](https://cli.angular.io/)
