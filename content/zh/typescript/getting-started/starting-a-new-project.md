---
date: 2024-01-20 18:04:38.783323-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u521B\u5EFA\u65B0\u7684TypeScript\u9879\
  \u76EE\u975E\u5E38\u76F4\u63A5\u3002\u5B89\u88C5Node.js\u548Cnpm\u540E\uFF0C\u6309\
  \u7167\u4EE5\u4E0B\u6B65\u9AA4\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.796388-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u521B\u5EFA\u65B0\u7684TypeScript\u9879\
  \u76EE\u975E\u5E38\u76F4\u63A5\u3002\u5B89\u88C5Node.js\u548Cnpm\u540E\uFF0C\u6309\
  \u7167\u4EE5\u4E0B\u6B65\u9AA4\uFF1A."
title: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE"
weight: 1
---

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
