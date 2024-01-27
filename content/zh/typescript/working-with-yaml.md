---
title:                "处理 YAML 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
为什么程序员要处理YAML？简单说，YAML是一种数据序列化格式，易读易写，常用于配置文件、数据交换。程序员用它因为它简洁且能轻松映射到数据结构。

## How to:
处理YAML，需要一个库。我们用`js-yaml`。先安装：

```bash
npm install js-yaml
```

然后，在TypeScript中使用：

```typescript
import * as yaml from 'js-yaml';
import { readFileSync } from 'fs';

// YAML文件读取
const doc = yaml.load(readFileSync('/path/to/your/file.yaml', 'utf8'));

console.log(doc);
```

输出是YAML文件的内容，转换成了JavaScript对象。

保存YAML：

```typescript
import * as yaml from 'js-yaml';
import { writeFileSync } from 'fs';

const obj = { name: '张三', age: 30, skills: ['TypeScript', 'YAML'] };

// 将对象保存为YAML格式
const yamlContent = yaml.dump(obj);
writeFileSync('/path/to/your/file.yaml', yamlContent, 'utf8');
```

这样就把对象写入了`file.yaml`。

## Deep Dive
YAML从2001年开始发展，是JSON的一个超集。相比XML和JSON，YAML更注重可读性和简洁。但并非所有情况下都最佳，大数据量或者性能关键的场景可能会选择JSON或者Protocol Buffers。

在TypeScript里，处理YAML需要一个类型声明来保证类型安全。如果有复杂的数据结构，可能会依赖接口或类来定义类型。例如：

```typescript
interface User {
  name: string;
  age: number;
  skills: string[];
}

const user: User = yaml.load(readFileSync('/path/to/user.yaml', 'utf8'));
```

请确保YAML结构与接口匹配，否则编译时会出错。

## See Also
- YAML官方网站: [yaml.org](https://yaml.org/)
- `js-yaml`库文档: [npm js-yaml](https://www.npmjs.com/package/js-yaml)
- TypeScript官方文档: [TypeScript](https://www.typescriptlang.org/) 

以上链接提供更多关于YAML和TypeScript的相关信息和高级使用案例。
