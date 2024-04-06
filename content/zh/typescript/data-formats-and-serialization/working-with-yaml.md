---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:50.985498-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728TypeScript\u4E2D\u5904\u7406YAML\u901A\
  \u5E38\u6D89\u53CA\u5C06YAML\u5185\u5BB9\u89E3\u6790\u4E3AJavaScript\u5BF9\u8C61\
  \uFF0C\u53EF\u80FD\u8FD8\u5305\u62EC\u5C06JavaScript\u5BF9\u8C61\u8F6C\u6362\u56DE\
  YAML\u3002\u8FD9\u9700\u8981\u4E00\u4E2A\u89E3\u6790\u5668\uFF1B\u4E00\u4E2A\u6D41\
  \u884C\u7684\u9009\u62E9\u662F`js-yaml`\uFF0C\u8FD9\u662F\u4E00\u4E2A\u53EF\u4EE5\
  \u8F7B\u677E\u96C6\u6210\u5230TypeScript\u9879\u76EE\u4E2D\u7684\u5E93\u3002 \u9996\
  \u5148\uFF0C\u5411\u60A8\u7684\u9879\u76EE\u6DFB\u52A0`js-yaml`\uFF1A."
lastmod: '2024-03-13T22:44:47.495223-06:00'
model: gpt-4-0125-preview
summary: "\u5728TypeScript\u4E2D\u5904\u7406YAML\u901A\u5E38\u6D89\u53CA\u5C06YAML\u5185\
  \u5BB9\u89E3\u6790\u4E3AJavaScript\u5BF9\u8C61\uFF0C\u53EF\u80FD\u8FD8\u5305\u62EC\
  \u5C06JavaScript\u5BF9\u8C61\u8F6C\u6362\u56DEYAML\u3002\u8FD9\u9700\u8981\u4E00\
  \u4E2A\u89E3\u6790\u5668\uFF1B\u4E00\u4E2A\u6D41\u884C\u7684\u9009\u62E9\u662F`js-yaml`\uFF0C\
  \u8FD9\u662F\u4E00\u4E2A\u53EF\u4EE5\u8F7B\u677E\u96C6\u6210\u5230TypeScript\u9879\
  \u76EE\u4E2D\u7684\u5E93."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

## 如何操作：
在TypeScript中处理YAML通常涉及将YAML内容解析为JavaScript对象，可能还包括将JavaScript对象转换回YAML。这需要一个解析器；一个流行的选择是`js-yaml`，这是一个可以轻松集成到TypeScript项目中的库。

### 安装js-yaml
首先，向您的项目添加`js-yaml`：

```bash
npm install js-yaml
```

### 将YAML解析为JavaScript对象
假设你有一个YAML文件`config.yaml`，内容如下：

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

您可以按如下方式读取并解析此文件为一个JavaScript对象：

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// 加载并解析YAML文件
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**示例输出：**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### 将JavaScript对象转换为YAML
如果您需要反过来，将JavaScript对象转换为YAML字符串，您可以如下使用`js-yaml`：

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Example",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**示例输出：**

```yaml
title: Example
is_published: true
author:
  name: Jane Doe
  age: 34
```

此代码段将JavaScript对象转换为YAML字符串并输出它。实际上，您可能会将此写回到文件，或在应用程序的其他部分使用它。
