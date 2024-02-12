---
title:                "使用YAML工作"
date:                  2024-02-03T19:26:50.985498-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
