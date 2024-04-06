---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:39.591715-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 JavaScript \u4E2D\uFF0C\u5904\u7406\
  \ YAML \u901A\u5E38\u6D89\u53CA\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\uFF0C\u56E0\u4E3A\
  \u8BE5\u8BED\u8A00\u6CA1\u6709\u5185\u7F6E\u7684 YAML \u89E3\u6790\u5668\u3002\u7528\
  \u4E8E\u6B64\u76EE\u7684\u6700\u6D41\u884C\u7684\u5E93\u4E4B\u4E00\u662F `js-yaml`\u3002\
  \u4F60\u53EF\u4EE5\u4F7F\u7528 `js-yaml` \u5C06 YAML \u89E3\u6790\u6210 JavaScript\
  \ \u5BF9\u8C61\uFF0C\u53CD\u4E4B\u4EA6\u7136\u3002 \u9996\u5148\uFF0C\u4F60\u9700\
  \u8981\u5B89\u88C5 `js-yaml`\uFF1A."
lastmod: '2024-04-05T22:38:47.376584-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 JavaScript \u4E2D\uFF0C\u5904\u7406\
  \ YAML \u901A\u5E38\u6D89\u53CA\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\uFF0C\u56E0\u4E3A\
  \u8BE5\u8BED\u8A00\u6CA1\u6709\u5185\u7F6E\u7684 YAML \u89E3\u6790\u5668\u3002\u7528\
  \u4E8E\u6B64\u76EE\u7684\u6700\u6D41\u884C\u7684\u5E93\u4E4B\u4E00\u662F `js-yaml`\u3002\
  \u4F60\u53EF\u4EE5\u4F7F\u7528 `js-yaml` \u5C06 YAML \u89E3\u6790\u6210 JavaScript\
  \ \u5BF9\u8C61\uFF0C\u53CD\u4E4B\u4EA6\u7136\u3002 \u9996\u5148\uFF0C\u4F60\u9700\
  \u8981\u5B89\u88C5 `js-yaml`\uFF1A."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

## 如何操作：
在 JavaScript 中，处理 YAML 通常涉及使用第三方库，因为该语言没有内置的 YAML 解析器。用于此目的最流行的库之一是 `js-yaml`。你可以使用 `js-yaml` 将 YAML 解析成 JavaScript 对象，反之亦然。

首先，你需要安装 `js-yaml`：

```bash
npm install js-yaml
```

然后，你可以在你的项目中使用它。下面是如何加载一个 YAML 文件并将其解析为 JavaScript 对象的方法：

```javascript
// 引入 js-yaml 模块
const yaml = require('js-yaml');
const fs   = require('fs');

// 从文件加载 YAML
try {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

如果你的 `config.yaml` 文件是这样的：

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

输出将是：

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

为了做反向操作，即将一个 JavaScript 对象转换成一个 YAML 字符串：

```javascript
const yaml = require('js-yaml');
const obj = {
  version: 1,
  services: {
    web: {
      image: "myapp/web:latest",
      ports: ["5000:5000"]
    }
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

这段代码将产生：

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

使用 `js-yaml`，你可以轻松地将 YAML 解析和序列化集成到你的 JavaScript 项目中，增强数据交换和配置管理的能力。
