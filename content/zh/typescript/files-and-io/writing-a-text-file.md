---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:32.978905-07:00
description: "\u5728TypeScript\u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u662F\u6570\
  \u636E\u6301\u4E45\u6027\u3001\u914D\u7F6E\u6216\u65E5\u5FD7\u751F\u6210\u7684\u5173\
  \u952E\u6280\u80FD\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\u884C\u6B64\u4EFB\u52A1\
  \uFF0C\u4EE5\u4FBF\u4E8E\u5728\u5E94\u7528\u7A0B\u5E8F\u5185\u5B58\u4E4B\u5916\u5B58\
  \u50A8\u548C\u64CD\u4F5C\u6570\u636E\uFF0C\u539F\u56E0\u53EF\u80FD\u662F\u6570\u636E\
  \u5206\u6790\u3001\u62A5\u544A\uFF0C\u6216\u4EC5\u4EC5\u662F\u5728\u4F1A\u8BDD\u4E4B\
  \u95F4\u4FDD\u5B58\u7528\u6237\u8BBE\u7F6E\u3002"
lastmod: '2024-02-25T18:49:45.055594-07:00'
model: gpt-4-0125-preview
summary: "\u5728TypeScript\u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u662F\u6570\u636E\
  \u6301\u4E45\u6027\u3001\u914D\u7F6E\u6216\u65E5\u5FD7\u751F\u6210\u7684\u5173\u952E\
  \u6280\u80FD\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\u884C\u6B64\u4EFB\u52A1\uFF0C\
  \u4EE5\u4FBF\u4E8E\u5728\u5E94\u7528\u7A0B\u5E8F\u5185\u5B58\u4E4B\u5916\u5B58\u50A8\
  \u548C\u64CD\u4F5C\u6570\u636E\uFF0C\u539F\u56E0\u53EF\u80FD\u662F\u6570\u636E\u5206\
  \u6790\u3001\u62A5\u544A\uFF0C\u6216\u4EC5\u4EC5\u662F\u5728\u4F1A\u8BDD\u4E4B\u95F4\
  \u4FDD\u5B58\u7528\u6237\u8BBE\u7F6E\u3002"
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么及为什么？
在TypeScript中写入文本文件是数据持久性、配置或日志生成的关键技能。程序员经常执行此任务，以便于在应用程序内存之外存储和操作数据，原因可能是数据分析、报告，或仅仅是在会话之间保存用户设置。

## 如何操作：
TypeScript本身并不直接处理文件操作，因为它编译为JavaScript，后者传统上在浏览器中运行，对文件系统的访问非常有限。然而，在Node.js环境中使用时，`fs`模块（文件系统）提供了写文件的功能。

### 使用Node.js的fs模块
首先，确保您在Node.js环境中工作。然后，使用`fs`模块来写文本文件。这里有一个基本示例：

```typescript
import * as fs from 'fs';

const data = 'Hello, world!';
const filePath = './message.txt';

fs.writeFile(filePath, data, 'utf8', (err) => {
    if (err) throw err;
    console.log('文件已保存！');
});
```

这将异步将“Hello, world!”写入`message.txt`。如果文件不存在，Node.js会创建它；如果它已存在，Node.js会覆写它。

对于同步文件写入，请使用`writeFileSync`：

```typescript
import * as fs from 'fs';

const data = 'Hello again, world!';
const filePath = './message.txt';

try {
    fs.writeFileSync(filePath, data, 'utf8');
    console.log('文件已保存！');
} catch (err) {
    console.error(err);
}
```

### 使用流行的第三方库
虽然原生的`fs`模块非常强大，但有些开发者更喜欢使用第三方库以获得额外的便利性和功能。`fs-extra`是一种流行的选择，它扩展了`fs`并使文件操作更加简单。

首先，您需要安装`fs-extra`：

```
npm install fs-extra
```

然后，在您的TypeScript文件中使用它来写文本内容：

```typescript
import * as fs from 'fs-extra';

const data = '这是fs-extra！';
const filePath = './extraMessage.txt';

// 使用async/await
async function writeFile() {
    try {
        await fs.writeFile(filePath, data, 'utf8');
        console.log('文件已经用fs-extra保存！');
    } catch (err) {
        console.error(err);
    }
}

writeFile();
```

这段代码片段做了和之前的`fs`示例相同的事情，但利用了`fs-extra`库，为处理promises提供了更清晰的语法。
