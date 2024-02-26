---
date: 2024-01-26 04:23:33.266053-07:00
description: "TOML\u3068\u306F\u3001Tom\u306E\u30AA\u30D3\u30D3\u30A2\u30B9\u30FB\u30DF\
  \u30CB\u30DE\u30EB\u30FB\u30E9\u30F3\u30B2\u30FC\u30B8\u306E\u7565\u3067\u3001\u8A2D\
  \u5B9A\u30D5\u30A1\u30A4\u30EB\u306E\u69CB\u9020\u3092\u5B9A\u7FA9\u3057\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304CTOML\u3092\u4F7F\u3046\u306E\u306F\
  \u3001\u8AAD\u307F\u66F8\u304D\u304C\u7C21\u5358\u3067\u30CF\u30C3\u30B7\u30E5\u30C6\
  \u30FC\u30D6\u30EB\u306B\u3046\u307E\u304F\u30DE\u30C3\u30D7\u3067\u304D\u308B\u305F\
  \u3081\u3001\u8A2D\u5B9A\u7528\u306B\u9069\u3057\u3066\u3044\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.650086-07:00'
model: gpt-4-0125-preview
summary: "TOML\u3068\u306F\u3001Tom\u306E\u30AA\u30D3\u30D3\u30A2\u30B9\u30FB\u30DF\
  \u30CB\u30DE\u30EB\u30FB\u30E9\u30F3\u30B2\u30FC\u30B8\u306E\u7565\u3067\u3001\u8A2D\
  \u5B9A\u30D5\u30A1\u30A4\u30EB\u306E\u69CB\u9020\u3092\u5B9A\u7FA9\u3057\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304CTOML\u3092\u4F7F\u3046\u306E\u306F\
  \u3001\u8AAD\u307F\u66F8\u304D\u304C\u7C21\u5358\u3067\u30CF\u30C3\u30B7\u30E5\u30C6\
  \u30FC\u30D6\u30EB\u306B\u3046\u307E\u304F\u30DE\u30C3\u30D7\u3067\u304D\u308B\u305F\
  \u3081\u3001\u8A2D\u5B9A\u7528\u306B\u9069\u3057\u3066\u3044\u307E\u3059\u3002"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
---

{{< edit_this_page >}}

## 何となぜ？
TOMLとは、Tomのオビビアス・ミニマル・ランゲージの略で、設定ファイルの構造を定義します。プログラマーがTOMLを使うのは、読み書きが簡単でハッシュテーブルにうまくマップできるため、設定用に適しています。

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
