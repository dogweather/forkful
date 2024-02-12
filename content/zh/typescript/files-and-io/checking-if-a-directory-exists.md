---
title:                "检查目录是否存在"
aliases:
- /zh/typescript/checking-if-a-directory-exists/
date:                  2024-02-03T19:08:41.349110-07:00
model:                 gpt-4-0125-preview
simple_title:         "检查目录是否存在"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
