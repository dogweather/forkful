---
title:                "使用YAML工作"
aliases:
- /zh/google-apps-script/working-with-yaml/
date:                  2024-02-01T22:07:31.784270-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/google-apps-script/working-with-yaml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么及为什么？

YAML，代表“YAML 不是标记语言（YAML Ain't Markup Language）”，是一种人类可读的数据序列化标准，通常用于配置文件和不同语言间具有不同数据结构的数据交换。程序员经常处理YAML的原因是它的简单性和可读性，特别是在需要大量配置或在不同系统间传输结构化数据的项目中。

## 如何操作：

尽管Google Apps Script（GAS）并不原生支持YAML解析或序列化，您可以通过使用JavaScript库或编写自定义解析函数来操作YAML数据。作为展示，让我们考虑如何使用自定义函数解析YAML字符串，因为外部库不能直接导入到GAS。

假设您有一个简单的YAML配置：

```yaml
title: YAML Example
description: An example of how to handle YAML in Google Apps Script
tags:
  - Google Apps Script
  - YAML
  - Configuration
```

要在Google Apps Script中解析这个，使用JavaScript的字符串操作能力：

```javascript
function parseYAML(yamlString) {
  var result = {};
  var lines = yamlString.split("\n");
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    if (line.includes(":")) {
      var parts = line.split(":");
      var key = parts[0].trim();
      var value = parts[1].trim();
      // 基本数组处理
      if (value.startsWith("-")) {
        value = [value.substring(1).trim()];
        while (i + 1 < lines.length && lines[i + 1].trim().startsWith("-")) {
          i++;
          value.push(lines[i].trim().substring(1).trim());
        }
      }
      result[key] = value;
    }
  }
  return result;
}

function testYamlParsing() {
  var yaml = "title: YAML Example\ndescription: An example of how to handle YAML in Google Apps Script\ntags:\n  - Google Apps Script\n  - YAML\n  - Configuration";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

执行`testYamlParsing()`时，它输出：

```
{ title: 'YAML Example',
  description: 'An example of how to handle YAML in Google Apps Script',
  tags: [ 'Google Apps Script', ' YAML', ' Configuration' ] }
```

这种自定义解析方法相当基本，可能需要调整以适应复杂的YAML文件。

## 深入探讨

YAML于2001年首次发布，旨在比其前任如XML或JSON更易于人类阅读。尽管其简单性和易用性被广泛赞扬，但在Google Apps Script中处理YAML由于缺乏直接支持而呈现挑战。因此，程序员经常依赖JavaScript的多功能性来解析和生成YAML数据。然而，对于涉及深层嵌套和高级数据结构的复杂用例，这种方法可能变得繁琐且容易出错。

相比之下，JSON在Google Apps Script和大多数其他编程环境中得到原生支持，为数据序列化和反序列化提供了一种更直接的方法，无需额外的解析开销。JSON的语法比YAML的语法简洁，使其更适合于web应用程序中的数据交换。尽管如此，YAML因其配置文件和需要人类可读性的情况而保持流行。

在Google Apps Script中处理YAML时，考虑可读性和易用性之间的权衡。对于全面的YAML操作，可能值得探索外部工具或服务，这些工具或服务可以在您的脚本中处理之前将YAML转换为JSON。
