---
aliases:
- /zh/typescript/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:41.349110-07:00
description: "\u5728TypeScript\u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\
  \u5BF9\u4E8E\u6587\u4EF6\u7BA1\u7406\u4EFB\u52A1\u81F3\u5173\u91CD\u8981\uFF0C\u4F8B\
  \u5982\u4ECE\u6587\u4EF6\u4E2D\u8BFB\u53D6\u6216\u5411\u6587\u4EF6\u5199\u5165\u6570\
  \u636E\uFF0C\u786E\u4FDD\u53EA\u5728\u6709\u6548\u76EE\u5F55\u4E0A\u6267\u884C\u64CD\
  \u4F5C\u3002\u8FD9\u4E2A\u64CD\u4F5C\u5BF9\u4E8E\u907F\u514D\u5C1D\u8BD5\u8BBF\u95EE\
  \u6216\u64CD\u4F5C\u4E0D\u5B58\u5728\u7684\u76EE\u5F55\u800C\u4EA7\u751F\u7684\u9519\
  \u8BEF\u81F3\u5173\u91CD\u8981\u3002"
lastmod: 2024-02-18 23:08:58.916643
model: gpt-4-0125-preview
summary: "\u5728TypeScript\u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u5BF9\
  \u4E8E\u6587\u4EF6\u7BA1\u7406\u4EFB\u52A1\u81F3\u5173\u91CD\u8981\uFF0C\u4F8B\u5982\
  \u4ECE\u6587\u4EF6\u4E2D\u8BFB\u53D6\u6216\u5411\u6587\u4EF6\u5199\u5165\u6570\u636E\
  \uFF0C\u786E\u4FDD\u53EA\u5728\u6709\u6548\u76EE\u5F55\u4E0A\u6267\u884C\u64CD\u4F5C\
  \u3002\u8FD9\u4E2A\u64CD\u4F5C\u5BF9\u4E8E\u907F\u514D\u5C1D\u8BD5\u8BBF\u95EE\u6216\
  \u64CD\u4F5C\u4E0D\u5B58\u5728\u7684\u76EE\u5F55\u800C\u4EA7\u751F\u7684\u9519\u8BEF\
  \u81F3\u5173\u91CD\u8981\u3002"
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在TypeScript中检查目录是否存在对于文件管理任务至关重要，例如从文件中读取或向文件写入数据，确保只在有效目录上执行操作。这个操作对于避免尝试访问或操作不存在的目录而产生的错误至关重要。

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
