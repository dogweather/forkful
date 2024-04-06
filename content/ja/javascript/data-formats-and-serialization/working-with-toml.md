---
date: 2024-01-26 04:23:33.266053-07:00
description: "\u4F7F\u3044\u65B9: JavaScript\u3067TOML\u3092\u6271\u3046\u306B\u306F\
  \u3001`@iarna/toml`\u306E\u3088\u3046\u306A\u30D1\u30FC\u30B5\u30FC\u304C\u5FC5\u8981\
  \u3067\u3059\u3002\u307E\u305A\u3001\u305D\u308C\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\
  \u30EB\u3057\u307E\u3059\uFF1A`npm install\u2026"
lastmod: '2024-04-05T21:53:43.494185-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\u3067TOML\u3092\u6271\u3046\u306B\u306F\u3001`@iarna/toml`\u306E\
  \u3088\u3046\u306A\u30D1\u30FC\u30B5\u30FC\u304C\u5FC5\u8981\u3067\u3059\u3002\u307E\
  \u305A\u3001\u305D\u308C\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u307E\u3059\
  \uFF1A`npm install @iarna/toml`\u3002\u6B21\u306B\u3001TOML\u6587\u5B57\u5217\u3092\
  JavaScript\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u30D1\u30FC\u30B9\u3059\u308B\
  \u304B\u3001JavaScript\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092TOML\u5F62\u5F0F\
  \u306B\u6587\u5B57\u5217\u5316\u3057\u307E\u3059\u3002"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

## 使い方:
JavaScriptでTOMLを扱うには、`@iarna/toml`のようなパーサーが必要です。まず、それをインストールします：`npm install @iarna/toml`。次に、TOML文字列をJavaScriptオブジェクトにパースするか、JavaScriptオブジェクトをTOML形式に文字列化します。

```javascript
const toml = require('@iarna/toml');

// TOML文字列をJSオブジェクトにパース
const tomlStr = `
title = "TOML Example"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// JSオブジェクトをTOML文字列に変換
const jsObject = {
  title: "TOML Example",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## 深堀り
TOMLは2013年にGitHubの共同創立者であるTom Preston-Wernerによって最初にリリースされました。INIなどの他のフォーマットよりも標準化されており、解析が容易であることを目指して設計されました。JSONやYAMLも代替案ですが、複雑すぎるか、柔軟性が高すぎることがあります。TOMLの利点は、シンプルでクリアなフォーマットが好まれる静的な設定にあります。その設計は、プロパティ名とその値に対応するキーと値をハッシュテーブルに直接マッピングできるようにします。より広い採用を望む場合は、異なるエコシステムのサポートにより、TOMLと他のフォーマット間で変換できるツールを統合する必要があるかもしれません。

## 参照
- 公式のTOML GitHubリポジトリ: https://github.com/toml-lang/toml
- TOML vs. YAML vs. JSONの比較: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- npm `@iarna/toml`パッケージ: https://www.npmjs.com/package/@iarna/toml
