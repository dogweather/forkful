---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:33.652354-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1F Google Apps Script\u306F\u672C\u8CEA\
  \u7684\u306BGoogle\u306E\u30A2\u30D7\u30EA\u306E\u30B9\u30A4\u30FC\u30C8\u306B\u30A2\
  \u30AF\u30BB\u30B9\u3067\u304D\u308BJavaScript\u306A\u306E\u3067\u3001Google Apps\
  \ Script\u3067\u76F4\u63A5\u7684\u306BTOML\u3092\u6271\u3046\u306B\u306F\u5C11\u3057\
  \u5DE5\u592B\u304C\u5FC5\u8981\u3067\u3059\u3002Google Apps\u2026"
lastmod: '2024-04-05T22:37:49.813555-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u306E\u3088\u3046\u306B\uFF1F Google Apps Script\u306F\u672C\u8CEA\
  \u7684\u306BGoogle\u306E\u30A2\u30D7\u30EA\u306E\u30B9\u30A4\u30FC\u30C8\u306B\u30A2\
  \u30AF\u30BB\u30B9\u3067\u304D\u308BJavaScript\u306A\u306E\u3067\u3001Google Apps\
  \ Script\u3067\u76F4\u63A5\u7684\u306BTOML\u3092\u6271\u3046\u306B\u306F\u5C11\u3057\
  \u5DE5\u592B\u304C\u5FC5\u8981\u3067\u3059\u3002Google Apps Script\u306F\u30CD\u30A4\
  \u30C6\u30A3\u30D6\u306BTOML\u306E\u30D1\u30FC\u30B7\u30F3\u30B0\u3092\u30B5\u30DD\
  \u30FC\u30C8\u3057\u3066\u3044\u307E\u305B\u3093\u304C\u3001JavaScript\u306E\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3092\u6D3B\u7528\u3059\u308B\u304B\u3001\u57FA\u672C\u7684\
  \u306A\u30CB\u30FC\u30BA\u306E\u305F\u3081\u306B\u7C21\u5358\u306A\u30D1\u30FC\u30B5\
  \u30FC\u3092\u66F8\u304F\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002 \u4F8B\u3068\
  \u3057\u3066\u3001\u30B7\u30F3\u30D7\u30EB\u306ATOML\u8A2D\u5B9A\u6587\u5B57\u5217\
  \u3092\u30D1\u30FC\u30B9\u3057\u3066\u307F\u307E\u3057\u3087\u3046\uFF1A."
title: "TOML\u3092\u6D3B\u7528\u3059\u308B"
weight: 39
---

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
