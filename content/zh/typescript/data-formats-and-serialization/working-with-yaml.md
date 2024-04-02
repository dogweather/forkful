---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:50.985498-07:00
description: "YAML\uFF0C\u4E00\u79CD\u65E8\u5728\u5BF9\u4EBA\u7C7B\u53CB\u597D\u7684\
  \u6570\u636E\u5E8F\u5217\u5316\u8BED\u8A00\uFF0C\u901A\u5E38\u7528\u4E8E\u914D\u7F6E\
  \u6587\u4EF6\u3001\u8FDB\u7A0B\u95F4\u6D88\u606F\u4F20\u9012\u548C\u6570\u636E\u5B58\
  \u50A8\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u4F9D\u8D56YAML\uFF0C\u662F\u56E0\
  \u4E3A\u5B83\u7684\u53EF\u8BFB\u6027\u548C\u6613\u7528\u6027\uFF0C\u7279\u522B\u662F\
  \u5728\u5904\u7406\u590D\u6742\u7684\u7ED3\u6784\u5316\u6570\u636E\u65F6\uFF0C\u8FD9\
  \u4F7F\u5176\u6210\u4E3ATypeScript\u5F00\u53D1\u7684\u5E94\u7528\u7A0B\u5E8F\u7684\
  \u7EDD\u4F73\u9009\u62E9\u3002"
lastmod: '2024-03-13T22:44:47.495223-06:00'
model: gpt-4-0125-preview
summary: "YAML\uFF0C\u4E00\u79CD\u65E8\u5728\u5BF9\u4EBA\u7C7B\u53CB\u597D\u7684\u6570\
  \u636E\u5E8F\u5217\u5316\u8BED\u8A00\uFF0C\u901A\u5E38\u7528\u4E8E\u914D\u7F6E\u6587\
  \u4EF6\u3001\u8FDB\u7A0B\u95F4\u6D88\u606F\u4F20\u9012\u548C\u6570\u636E\u5B58\u50A8\
  \u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u4F9D\u8D56YAML\uFF0C\u662F\u56E0\u4E3A\
  \u5B83\u7684\u53EF\u8BFB\u6027\u548C\u6613\u7528\u6027\uFF0C\u7279\u522B\u662F\u5728\
  \u5904\u7406\u590D\u6742\u7684\u7ED3\u6784\u5316\u6570\u636E\u65F6\uFF0C\u8FD9\u4F7F\
  \u5176\u6210\u4E3ATypeScript\u5F00\u53D1\u7684\u5E94\u7528\u7A0B\u5E8F\u7684\u7EDD\
  \u4F73\u9009\u62E9\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

## 什么和为什么？
YAML，一种旨在对人类友好的数据序列化语言，通常用于配置文件、进程间消息传递和数据存储。程序员之所以依赖YAML，是因为它的可读性和易用性，特别是在处理复杂的结构化数据时，这使其成为TypeScript开发的应用程序的绝佳选择。

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
