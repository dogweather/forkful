---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:41.349110-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5F53\u5728Node.js\u73AF\u5883\u4E2D\
  \u8FD0\u884CTypeScript\u65F6\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528`fs`\u6A21\u5757\
  \u6765\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\uFF0C\u8BE5\u6A21\u5757\u63D0\
  \u4F9B\u4E86`existsSync()`\u51FD\u6570\u6216\u5F02\u6B65\u7684`access()`\u51FD\u6570\
  \u7ED3\u5408`constants.F_OK`\u3002 #."
lastmod: '2024-03-13T22:44:47.488227-06:00'
model: gpt-4-0125-preview
summary: "\u5F53\u5728Node.js\u73AF\u5883\u4E2D\u8FD0\u884CTypeScript\u65F6\uFF0C\u4F60\
  \u53EF\u4EE5\u4F7F\u7528`fs`\u6A21\u5757\u6765\u68C0\u67E5\u76EE\u5F55\u662F\u5426\
  \u5B58\u5728\uFF0C\u8BE5\u6A21\u5757\u63D0\u4F9B\u4E86`existsSync()`\u51FD\u6570\
  \u6216\u5F02\u6B65\u7684`access()`\u51FD\u6570\u7ED3\u5408`constants.F_OK`."
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

## 如何操作：
当在Node.js环境中运行TypeScript时，你可以使用`fs`模块来检查目录是否存在，该模块提供了`existsSync()`函数或异步的`access()`函数结合`constants.F_OK`。

### 使用`fs.existsSync()`：
```typescript
import { existsSync } from 'fs';

const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log('目录存在。');
} else {
  console.log('目录不存在。');
}
```

### 使用`fs.access()`搭配`fs.constants.F_OK`：
```typescript
import { access, constants } from 'fs';

const directoryPath = './path/to/directory';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('目录不存在。');
    return;
  }
  console.log('目录存在。');
});
```

**两种方法的示例输出**，假设目录确实存在：
```
目录存在。
```

如果不存在：
```
目录不存在。
```

### 使用第三方库 - `fs-extra`：
`fs-extra`是一个受欢迎的第三方库，它增强了内置的`fs`模块，并提供了更方便的函数。

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './path/to/directory';

pathExists(directoryPath).then(exists => {
  console.log(`目录存在: ${exists}`);
});
```

**示例输出** 当目录存在时：
```
目录存在: true
```

如果不存在：
```
目录不存在: false
```
