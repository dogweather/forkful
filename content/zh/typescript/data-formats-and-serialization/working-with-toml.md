---
date: 2024-01-26 04:27:07.830457-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u9996\u5148\uFF0C\u4F60\u9700\u8981\u4E00\
  \u4E2ATOML\u89E3\u6790\u5668\u3002`@iarna/toml`\u662F\u4E00\u4E2A\u6D41\u884C\u7684\
  \u9009\u62E9\u3002\u4F7F\u7528npm\u5B89\u88C5\u5B83\uFF1A`npm install @iarna/toml\
  \ --save`\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u8BFB\u53D6TOML\u6587\u4EF6\u5E76\u5C06\
  \u5176\u89E3\u6790\u4E3AJavaScript\u5BF9\u8C61\u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T21:53:47.821924-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528TOML"
weight: 39
---

## 如何操作：
首先，你需要一个TOML解析器。`@iarna/toml`是一个流行的选择。使用npm安装它：`npm install @iarna/toml --save`。以下是如何读取TOML文件并将其解析为JavaScript对象的方法：

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
如果`config.toml`包含：
```
[server]
port = 8080
```
输出将会是：
```
{ server: { port: 8080 } }
```
而且，向TOML文件写入数据同样直接：
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
运行此代码会以TOML格式将对象写入`config.toml`。

## 深入探索
TOML由GitHub的共同创始人Tom Preston-Werner在2013年左右创建，以回应他认为INI或YAML等其他格式的限制。它旨在消除歧义，并易于解析为数据结构，因此，它是配置文件的首选。像JSON这样的替代方案缺少注释，而YAML更加复杂。TOML以其简单性和清晰表示复杂数据层次结构的能力而脱颖而出。

在底层，当你在TypeScript中解析TOML时，你正在将文本数据转换为语言可以操作的结构化格式。这涉及到词法分析（将原始文本转换为令牌）和解析（建立内部数据结构）；`@iarna/toml`可以无缝处理这两个过程。表情符号支持是一个有趣的触摸，显示了TOML以用户为中心的方法。

## 另请参阅
- TOML官方规范：https://toml.io/en/
- `@iarna/toml`包：https://www.npmjs.com/package/@iarna/toml
- TOML、YAML和JSON之间的比较：https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
