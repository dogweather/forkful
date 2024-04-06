---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:14.142194-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u7531\u4E8E Google Apps Script \u672C\u8D28\
  \u4E0A\u662F\u53EF\u4EE5\u8BBF\u95EE Google \u5E94\u7528\u7A0B\u5E8F\u5957\u4EF6\
  \u7684 JavaScript\uFF0C\u56E0\u6B64\u76F4\u63A5\u5728 Google Apps Script \u4E2D\u5904\
  \u7406 TOML \u9700\u8981\u4E00\u4E9B\u5DE7\u601D\u3002Google Apps Script \u5E76\u4E0D\
  \u539F\u751F\u652F\u6301 TOML \u89E3\u6790\uFF0C\u4F46\u4F60\u53EF\u4EE5\u5229\u7528\
  \u2026"
lastmod: '2024-04-05T22:38:46.415230-06:00'
model: gpt-4-0125-preview
summary: "\u7531\u4E8E Google Apps Script \u672C\u8D28\u4E0A\u662F\u53EF\u4EE5\u8BBF\
  \u95EE Google \u5E94\u7528\u7A0B\u5E8F\u5957\u4EF6\u7684 JavaScript\uFF0C\u56E0\u6B64\
  \u76F4\u63A5\u5728 Google Apps Script \u4E2D\u5904\u7406 TOML \u9700\u8981\u4E00\
  \u4E9B\u5DE7\u601D\u3002Google Apps Script \u5E76\u4E0D\u539F\u751F\u652F\u6301\
  \ TOML \u89E3\u6790\uFF0C\u4F46\u4F60\u53EF\u4EE5\u5229\u7528 JavaScript \u5E93\u6216\
  \u4E3A\u57FA\u672C\u9700\u6C42\u7F16\u5199\u4E00\u4E2A\u7B80\u5355\u7684\u89E3\u6790\
  \u5668\u3002"
title: "\u4F7F\u7528TOML\u5DE5\u4F5C"
weight: 39
---

## 如何操作:
由于 Google Apps Script 本质上是可以访问 Google 应用程序套件的 JavaScript，因此直接在 Google Apps Script 中处理 TOML 需要一些巧思。Google Apps Script 并不原生支持 TOML 解析，但你可以利用 JavaScript 库或为基本需求编写一个简单的解析器。

让我们以解析一个简单的 TOML 配置字符串为例:

```javascript
// TOML 字符串
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// 一个简单的 TOML 到 JSON 的解析函数
function parseTOML(tomlStr) {
  var result = {};
  var currentSection = result;
  tomlStr.split(/\r?\n/).forEach(line => {
    line = line.trim();
    if (line.startsWith('[')) { // 新部分
      var sectionName = line.replace(/\[|\]/g, '');
      result[sectionName] = {};
      currentSection = result[sectionName];
    } else if (line) {
      var keyValue = line.split('=').map(part => part.trim());
      var key = keyValue[0];
      var value = eval(keyValue[1]); // 为了简单起见使用 eval; 在生产代码中请注意
      currentSection[key] = value;
    }
  });
  return result;
}

// 测试解析器
var configObject = parseTOML(tomlString);
console.log(configObject);

```

`console.log` 的示例输出会类似于一个 JSON 对象，使得在 Google Apps Script 中访问配置属性变得更加容易：

```json
{
  "database": {
    "server": "192.168.1.1",
    "ports": [8001, 8001, 8002],
    "connection_max": 5000,
    "enabled": true
  }
}
```

## 深入了解
TOML 由 GitHub 的创始人之一 Tom Preston-Werner 创建，旨在比 JSON 更适合配置文件，同时保持能被无歧义地解析的能力。它的目标是尽可能简单，这一目标非常符合许多开发项目追求在代码库中的简洁性和可读性的理念。

在 Google Apps Script 的上下文中，使用 TOML 可能会引入一些额外开销，鉴于缺乏直接支持和必须手动解析或通过第三方库进行解析的必要性。对于较小的项目或那些未深度集成到 Google 生态系统中的项目，使用 JSON 或甚至在脚本属性中使用简单的键值对结构可能足够，并且更直接简单。然而，对于那些优先考虑人类可读配置文件且已经承诺使用 TOML 的应用程序来说，通过自定义脚本集成 TOML 解析增加了一层有用的灵活性和可维护性，而不偏离首选的配置范式。
