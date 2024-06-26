---
date: 2024-01-26 04:23:41.944145-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728JavaScript\u4E2D\u4F7F\u7528\
  TOML\uFF0C\u4F60\u9700\u8981\u4E00\u4E2A\u89E3\u6790\u5668\uFF0C\u6BD4\u5982`@iarna/toml`\u3002\
  \u9996\u5148\uFF0C\u5B89\u88C5\u5B83\uFF1A`npm install @iarna/toml`\u3002\u7136\u540E\
  \uFF0C\u89E3\u6790\u4E00\u4E2ATOML\u5B57\u7B26\u4E32\u4E3AJavaScript\u5BF9\u8C61\
  \uFF0C\u6216\u5C06JavaScript\u5BF9\u8C61\u5E8F\u5217\u5316\u4E3ATOML\u683C\u5F0F\
  \u3002"
lastmod: '2024-04-05T22:38:47.379855-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728JavaScript\u4E2D\u4F7F\u7528\
  TOML\uFF0C\u4F60\u9700\u8981\u4E00\u4E2A\u89E3\u6790\u5668\uFF0C\u6BD4\u5982`@iarna/toml`\u3002\
  \u9996\u5148\uFF0C\u5B89\u88C5\u5B83\uFF1A`npm install @iarna/toml`\u3002\u7136\u540E\
  \uFF0C\u89E3\u6790\u4E00\u4E2ATOML\u5B57\u7B26\u4E32\u4E3AJavaScript\u5BF9\u8C61\
  \uFF0C\u6216\u5C06JavaScript\u5BF9\u8C61\u5E8F\u5217\u5316\u4E3ATOML\u683C\u5F0F\
  \u3002"
title: "\u4F7F\u7528TOML"
weight: 39
---

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
