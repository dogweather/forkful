---
date: 2024-01-26 04:23:41.944145-07:00
description: "TOML\uFF0C\u5373Tom's Obvious, Minimal Language\uFF08\u6C64\u59C6\u7684\
  \u663E\u800C\u6613\u89C1\u7684\u6700\u5C0F\u8BED\u8A00\uFF09\uFF0C\u5B9A\u4E49\u4E86\
  \u5982\u4F55\u7ED3\u6784\u5316\u914D\u7F6E\u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u4E4B\
  \u6240\u4EE5\u4F7F\u7528TOML\uFF0C\u662F\u56E0\u4E3A\u5176\u6613\u4E8E\u9605\u8BFB\
  \u3001\u7F16\u5199\uFF0C\u5E76\u4E14\u53EF\u4EE5\u5F88\u597D\u5730\u6620\u5C04\u5230\
  \u54C8\u5E0C\u8868\uFF0C\u56E0\u6B64\u6210\u4E3A\u914D\u7F6E\u7684\u9996\u9009\u3002"
lastmod: '2024-02-25T18:49:45.800711-07:00'
model: gpt-4-0125-preview
summary: "TOML\uFF0C\u5373Tom's Obvious, Minimal Language\uFF08\u6C64\u59C6\u7684\u663E\
  \u800C\u6613\u89C1\u7684\u6700\u5C0F\u8BED\u8A00\uFF09\uFF0C\u5B9A\u4E49\u4E86\u5982\
  \u4F55\u7ED3\u6784\u5316\u914D\u7F6E\u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\
  \u4EE5\u4F7F\u7528TOML\uFF0C\u662F\u56E0\u4E3A\u5176\u6613\u4E8E\u9605\u8BFB\u3001\
  \u7F16\u5199\uFF0C\u5E76\u4E14\u53EF\u4EE5\u5F88\u597D\u5730\u6620\u5C04\u5230\u54C8\
  \u5E0C\u8868\uFF0C\u56E0\u6B64\u6210\u4E3A\u914D\u7F6E\u7684\u9996\u9009\u3002"
title: "\u4F7F\u7528TOML"
---

{{< edit_this_page >}}

## 何为TOML以及为何使用？

TOML，即Tom's Obvious, Minimal Language（汤姆的显而易见的最小语言），定义了如何结构化配置文件。程序员之所以使用TOML，是因为其易于阅读、编写，并且可以很好地映射到哈希表，因此成为配置的首选。

## 如何操作：
要在JavaScript中使用TOML，你需要一个解析器，比如`@iarna/toml`。首先，安装它：`npm install @iarna/toml`。然后，解析一个TOML字符串为JavaScript对象，或将JavaScript对象序列化为TOML格式。

```javascript
const toml = require('@iarna/toml');

// 解析 TOML 字符串为 JS 对象
const tomlStr = `
title = "TOML 示例"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// 将 JS 对象转换为 TOML 字符串
const jsObject = {
  title: "TOML 示例",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## 深入了解
TOML 由GitHub的联合创始人汤姆·普雷斯顿-沃纳（Tom Preston-Werner）在2013年首次发布。它旨在超越其他格式，如INI，因为具有更标准化且更易于解析的特点。JSON和YAML是替代品，但可能太复杂或太灵活。 TOML的优势在于静态配置，其中一个简单、清晰的格式是首选。其设计允许直接映射到哈希表中，其键和值分别对应于属性名称及其值。为了更广泛的采用，你可能需要集成工具以在TOML与其他格式之间转换，因为不同的生态系统支持度不同。

## 参见
- 官方TOML GitHub仓库: https://github.com/toml-lang/toml
- TOML与YAML与JSON的比较: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- npm `@iarna/toml` 包: https://www.npmjs.com/package/@iarna/toml
