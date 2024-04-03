---
date: 2024-01-26 04:27:07.830457-07:00
description: "TOML\uFF0C\u5373Tom's Obvious, Minimal Language\u7684\u7F29\u5199\uFF0C\
  \u662F\u4E00\u79CD\u7C7B\u4F3C\u4E8EJSON\u6216YAML\u7684\u6570\u636E\u5E8F\u5217\
  \u5316\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u4F7F\u7528\u5B83\uFF0C\
  \u662F\u56E0\u4E3A\u5B83\u5177\u6709\u4EBA\u7C7B\u53EF\u8BFB\u6027\u548C\u76F4\u63A5\
  \u6620\u5C04\u5230\u6570\u636E\u7C7B\u578B\u7684\u7279\u70B9\uFF0C\u4F7F\u5176\u6210\
  \u4E3A\u914D\u7F6E\u6587\u4EF6\u548C\u6570\u636E\u4EA4\u6362\u7684\u9996\u9009\u3002"
lastmod: '2024-03-13T22:44:47.499398-06:00'
model: gpt-4-0125-preview
summary: "TOML\uFF0C\u5373Tom's Obvious, Minimal Language\u7684\u7F29\u5199\uFF0C\u662F\
  \u4E00\u79CD\u7C7B\u4F3C\u4E8EJSON\u6216YAML\u7684\u6570\u636E\u5E8F\u5217\u5316\
  \u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u4F7F\u7528\u5B83\uFF0C\u662F\
  \u56E0\u4E3A\u5B83\u5177\u6709\u4EBA\u7C7B\u53EF\u8BFB\u6027\u548C\u76F4\u63A5\u6620\
  \u5C04\u5230\u6570\u636E\u7C7B\u578B\u7684\u7279\u70B9\uFF0C\u4F7F\u5176\u6210\u4E3A\
  \u914D\u7F6E\u6587\u4EF6\u548C\u6570\u636E\u4EA4\u6362\u7684\u9996\u9009\u3002."
title: "\u4F7F\u7528TOML"
weight: 39
---

## 什么 & 为什么？
TOML，即Tom's Obvious, Minimal Language的缩写，是一种类似于JSON或YAML的数据序列化格式。程序员之所以使用它，是因为它具有人类可读性和直接映射到数据类型的特点，使其成为配置文件和数据交换的首选。

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
