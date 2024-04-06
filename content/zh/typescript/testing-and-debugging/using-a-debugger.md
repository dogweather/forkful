---
date: 2024-01-26 04:11:31.911706-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728TypeScript\u4E2D\u5F00\u59CB\
  \u4F7F\u7528\u8C03\u8BD5\u5668\uFF0C\u4F60\u6240\u9700\u8981\u7684\u53EA\u662F\u4E00\
  \u4E2A\u652F\u6301\u7684IDE\uFF08\u5982Visual Studio Code\uFF09\u548C\u4E00\u4E2A\
  `launch.json`\u914D\u7F6E\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u9488\u5BF9Node.js\u5E94\
  \u7528\u7A0B\u5E8F\u7684\u5FEB\u901F\u793A\u4F8B\uFF1A."
lastmod: '2024-04-05T22:38:46.635049-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728TypeScript\u4E2D\u5F00\u59CB\u4F7F\
  \u7528\u8C03\u8BD5\u5668\uFF0C\u4F60\u6240\u9700\u8981\u7684\u53EA\u662F\u4E00\u4E2A\
  \u652F\u6301\u7684IDE\uFF08\u5982Visual Studio Code\uFF09\u548C\u4E00\u4E2A`launch.json`\u914D\
  \u7F6E\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u9488\u5BF9Node.js\u5E94\u7528\u7A0B\u5E8F\
  \u7684\u5FEB\u901F\u793A\u4F8B\uFF1A."
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

## 如何操作：
要在TypeScript中开始使用调试器，你所需要的只是一个支持的IDE（如Visual Studio Code）和一个`launch.json`配置。这里有一个针对Node.js应用程序的快速示例：

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Hello, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

要调试此程序，需在`.vscode`文件夹下创建一个`launch.json`文件：

```JSON
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "node",
            "request": "launch",
            "name": "启动程序",
            "skipFiles": ["<node_internals>/**"],
            "program": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

接下来，在IDE中的`greet`函数行号的左侧点击设置一个断点。按F5开始调试，观察你的应用在断点处暂停。现在，你可以轻松地悬停在变量上，观察表达式，并逐步执行你的代码。

## 深入了解
在集成开发环境（IDEs）变得更加先进之前，调试通常是通过打印语句（也就是`console.log`调试）完成的。它勉强能用，但就像蒙着眼睛在干草堆里找针一样。

现代调试器就像是故障排除的瑞士军刀。随着TypeScript和Node.js的发展，有各种各样的调试器可用，从内置的Node.js检查器到用于客户端调试的浏览器开发工具。

Node.js检查器通过附加到你的正在运行的应用程序来工作; 它通过Chrome DevTools协议进行通信，将你的Chrome浏览器变成一个强大的调试控制台。与传统命令行调试实践相比，这种集成允许进行视觉交互且详细的调试会话。

## 另请参阅
想了解更多信息以及一些专业提示，请查看：

- [Visual Studio Code中的TypeScript调试](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Node.js调试指南](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Chrome开发者工具文档](https://developers.google.com/web/tools/chrome-devtools)
