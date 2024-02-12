---
title:                "使用YAML工作"
date:                  2024-02-03T19:25:39.591715-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

YAML，即 "YAML Ain't Markup Language"（YAML 不是标记语言）的缩写，是一种人类可读的数据序列化格式。程序员经常使用它来处理配置文件和跨语言的数据交换，因为与 JSON 或 XML 相比，它的简单性和可读性更强。

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
