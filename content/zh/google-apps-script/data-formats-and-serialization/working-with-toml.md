---
title:                "使用TOML工作"
aliases:
- /zh/google-apps-script/working-with-toml.md
date:                  2024-02-01T22:06:14.142194-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用TOML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/google-apps-script/working-with-toml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?

TOML，即 Tom's Obvious, Minimal Language（汤姆的显而易见的最简语言），是一种配置文件格式，因其清晰的语义而易于阅读。程序员经常使用它来处理应用程序中的配置文件，因为它直接且易于人类阅读，使得跨不同环境下应用程序设置和配置的管理变得无缝。

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
