---
title:                "处理 YAML 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么?)
YAML是一种数据序列化格式，易于人类阅读，常用于配置文件。程序员用它因为它简单、清晰，与JSON相比，更适合配置。

## How to: (怎么做)
使用JavaScript处理YAML通常需引入第三方库，如`js-yaml`。先通过NPM安装：`npm install js-yaml`。再看代码示例：

```Javascript
const yaml = require('js-yaml');
const fs = require('fs');

// 解析YAML字符串
const yamlStr = `
foo: bar
baz:
  - qux
  - quxx
`;
const data = yaml.load(yamlStr);
console.log(data);

// 输出: { foo: 'bar', baz: [ 'qux', 'quxx' ] }

// 读取YAML文件
const doc = yaml.load(fs.readFileSync('/path/to/file.yml', 'utf8'));
console.log(doc);

// 将JavaScript对象转换为YAML字符串
const yamlOutput = yaml.dump(data);
console.log(yamlOutput);

// 输出:
// foo: bar
// baz:
//   - qux
//   - quxx
```

## Deep Dive (深入解析)
YAML，即YAML Ain't Markup Language（YAML不是标记语言），起源于2001年，设计为XML的简化版。JSON是YAML的一个子集；所有JSON文件都是合法的YAML。相比XML和JSON，YAML的语法更少，易于人眼识别。但在性能和通用性上，JSON更有优势。JS处理YAML时，`js-yaml`是比较受欢迎的第三方库，同时，`yamljs`也是不错的选择。

## See Also (另请参阅)
- YAML官方网站: [https://yaml.org](https://yaml.org)
- js-yaml GitHub 仓库: [https://github.com/nodeca/js-yaml](https://github.com/nodeca/js-yaml)
- YAML和JSON的比较: [https://en.wikipedia.org/wiki/YAML#JSON](https://en.wikipedia.org/wiki/YAML#JSON)
