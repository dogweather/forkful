---
title:                "TOMLを扱う方法"
aliases: - /ja/javascript/working-with-toml.md
date:                  2024-01-26T04:23:33.266053-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/working-with-toml.md"
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
