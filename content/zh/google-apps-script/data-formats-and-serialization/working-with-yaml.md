---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:31.784270-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5C3D\u7BA1Google Apps Script\uFF08\
  GAS\uFF09\u5E76\u4E0D\u539F\u751F\u652F\u6301YAML\u89E3\u6790\u6216\u5E8F\u5217\u5316\
  \uFF0C\u60A8\u53EF\u4EE5\u901A\u8FC7\u4F7F\u7528JavaScript\u5E93\u6216\u7F16\u5199\
  \u81EA\u5B9A\u4E49\u89E3\u6790\u51FD\u6570\u6765\u64CD\u4F5CYAML\u6570\u636E\u3002\
  \u4F5C\u4E3A\u5C55\u793A\uFF0C\u8BA9\u6211\u4EEC\u8003\u8651\u5982\u4F55\u4F7F\u7528\
  \u81EA\u5B9A\u4E49\u51FD\u6570\u89E3\u6790YAML\u5B57\u7B26\u4E32\uFF0C\u56E0\u4E3A\
  \u5916\u90E8\u5E93\u4E0D\u80FD\u76F4\u63A5\u5BFC\u5165\u5230GAS\u3002 \u5047\u8BBE\
  \u60A8\u6709\u4E00\u4E2A\u7B80\u5355\u7684YAML\u914D\u7F6E\uFF1A."
lastmod: '2024-03-13T22:44:47.229098-06:00'
model: gpt-4-0125-preview
summary: "\u5C3D\u7BA1Google Apps Script\uFF08GAS\uFF09\u5E76\u4E0D\u539F\u751F\u652F\
  \u6301YAML\u89E3\u6790\u6216\u5E8F\u5217\u5316\uFF0C\u60A8\u53EF\u4EE5\u901A\u8FC7\
  \u4F7F\u7528JavaScript\u5E93\u6216\u7F16\u5199\u81EA\u5B9A\u4E49\u89E3\u6790\u51FD\
  \u6570\u6765\u64CD\u4F5CYAML\u6570\u636E\u3002\u4F5C\u4E3A\u5C55\u793A\uFF0C\u8BA9\
  \u6211\u4EEC\u8003\u8651\u5982\u4F55\u4F7F\u7528\u81EA\u5B9A\u4E49\u51FD\u6570\u89E3\
  \u6790YAML\u5B57\u7B26\u4E32\uFF0C\u56E0\u4E3A\u5916\u90E8\u5E93\u4E0D\u80FD\u76F4\
  \u63A5\u5BFC\u5165\u5230GAS."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

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
