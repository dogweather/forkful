---
title:                "TOMLを活用する"
aliases: - /ja/google-apps-script/working-with-toml.md
date:                  2024-02-01T22:06:33.652354-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを活用する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/working-with-toml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

TOMLは、Tom's Obvious, Minimal Languageの略で、明確なセマンティクスのために読みやすい設定ファイルフォーマットです。プログラマーはそれをアプリケーションの設定ファイルによく使用します。それは直接的で人間が読みやすいため、異なる環境間でのアプリケーションの設定と構成の管理をシームレスにします。

## どのように？

Google Apps Scriptは本質的にGoogleのアプリのスイートにアクセスできるJavaScriptなので、Google Apps Scriptで直接的にTOMLを扱うには少し工夫が必要です。Google Apps ScriptはネイティブにTOMLのパーシングをサポートしていませんが、JavaScriptのライブラリを活用するか、基本的なニーズのために簡単なパーサーを書くことができます。

例として、シンプルなTOML設定文字列をパースしてみましょう：

```javascript
// TOML文字列
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// シンプルなTOMLからJSONへのパーサー関数
function parseTOML(tomlStr) {
  var result = {};
  var currentSection = result;
  tomlStr.split(/\r?\n/).forEach(line => {
    line = line.trim();
    if (line.startsWith('[')) { // 新しいセクション
      var sectionName = line.replace(/\[|\]/g, '');
      result[sectionName] = {};
      currentSection = result[sectionName];
    } else if (line) {
      var keyValue = line.split('=').map(part => part.trim());
      var key = keyValue[0];
      var value = eval(keyValue[1]); // 単純さのためにevalを使用; 本番コードでは注意
      currentSection[key] = value;
    }
  });
  return result;
}

// パーサーをテスト
var configObject = parseTOML(tomlString);
console.log(configObject);
```

`console.log`からのサンプル出力は、JSONオブジェクトに似ており、Google Apps Script内で構成プロパティにアクセスするのを容易にします：

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

## 詳細分析

TOMLは、GitHubの創設者の一人であるTom Preston-Wernerによって作られ、設定ファイルに対してJSONよりも人間に優しいが、明確に解析可能であることを目指しています。できるだけシンプルであることを目標とし、多くの開発プロジェクトがそのコードベースで単純さと読みやすさを求める理念とよく一致しています。

Google Apps Scriptのコンテキストで、TOMLを使うには直接のサポートの欠如と手動またはサードパーティのライブラリを通じてのパースが必要とされるため、いくらかのオーバーヘッドがあります。小規模なプロジェクトやGoogleのエコシステムに深く統合されていないプロジェクトでは、代わりにJSONやスクリプトのプロパティ内のシンプルなキー・バリューペアの構造が十分であり、実装がより直接的である可能性があります。しかし、人間に優しい設定ファイルを優先し、すでにTOMLにコミットしているアプリケーションの場合、カスタムスクリプトを通じたTOMLパーシングの統合は、好ましい設定パラダイムから逸脱することなく、有用な柔軟性と保守性の層を追加します。
