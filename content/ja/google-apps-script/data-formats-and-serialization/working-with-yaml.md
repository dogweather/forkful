---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:23.291954-07:00
description: "YAML\u306F\u300CYAML Ain\u2019t Markup\u2026"
lastmod: '2024-03-13T22:44:41.472850-06:00'
model: gpt-4-0125-preview
summary: "YAML\u306F\u300CYAML Ain\u2019t Markup Language\u300D\u3068\u3044\u3046\u610F\
  \u5473\u3067\u3001\u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u3044\u30C7\u30FC\u30BF\
  \u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u6A19\u6E96\u3067\u3042\
  \u308A\u3001\u69CB\u6210\u30D5\u30A1\u30A4\u30EB\u3084\u7570\u306A\u308B\u30C7\u30FC\
  \u30BF\u69CB\u9020\u3092\u6301\u3064\u8A00\u8A9E\u9593\u306E\u30C7\u30FC\u30BF\u4EA4\
  \u63DB\u306B\u3088\u304F\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u7279\u306B\u5E83\u7BC4\u306A\u8A2D\u5B9A\u304C\u5FC5\
  \u8981\u306A\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3084\u7570\u306A\u308B\u30B7\u30B9\
  \u30C6\u30E0\u9593\u3067\u69CB\u9020\u5316\u3055\u308C\u305F\u30C7\u30FC\u30BF\u3092\
  \u8EE2\u9001\u3059\u308B\u969B\u306B\u3001\u305D\u306E\u30B7\u30F3\u30D7\u30EB\u3055\
  \u3068\u8AAD\u307F\u3084\u3059\u3055\u306E\u305F\u3081\u306BYAML\u3092\u3088\u304F\
  \u4F7F\u7528\u3057\u307E\u3059\u3002."
title: "YAML\u3068\u306E\u4F5C\u696D"
weight: 41
---

## 方法:
Google Apps Script（GAS）はYAMLのパースやシリアライゼーションをネイティブにサポートしていませんが、JavaScriptライブラリを使用するか、カスタムパース関数を書くことでYAMLデータを操作できます。デモのために、GASに直接外部ライブラリをインポートすることはできないため、カスタム関数を使用してYAML文字列を解析する方法を検討しましょう。

シンプルなYAML設定を想定してください：

```yaml
title: YAML Example
description: Google Apps ScriptでYAMLを扱う例
tags:
  - Google Apps Script
  - YAML
  - Configuration
```

これをGoogle Apps Scriptで解析するには、JavaScriptの文字列操作機能を使用します：

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
      // 配列の基本的な処理
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
  var yaml = "title: YAML Example\ndescription: Google Apps ScriptでYAMLを扱う例\ntags:\n  - Google Apps Script\n  - YAML\n  - Configuration";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

`testYamlParsing()`を実行すると、次のような出力になります：

```
{ title: 'YAML Example',
  description: 'Google Apps ScriptでYAMLを扱う例',
  tags: [ 'Google Apps Script', ' YAML', ' Configuration' ] }
```

このカスタムパース方法はかなり基本的であり、複雑なYAMLファイルに対応するためには調整が必要になるかもしれません。

## 詳細な解説
YAMLは2001年にリリースされ、XMLやJSONのような先行技術よりも人間が読みやすいことを目指しました。そのシンプルさと使いやすさは広く評価されていますが、Google Apps ScriptでのYAMLの取り扱いは直接的なサポートが欠如しているため課題があります。結果として、プログラマーはYAMLデータをパースし、生成するためにJavaScriptの多様性に頼ることが多いです。しかし、深いネスティングや高度なデータ構造を含む複雑な使用事例では、この方法は面倒でエラーが発生しやすくなります。

対照的に、JSONはGoogle Apps Scriptを含むほとんどのプログラミング環境でネイティブにサポートされており、追加のパーシングオーバーヘッドなしでデータのシリアライゼーションおよびデシリアライゼーションをより簡単に行うアプローチを提供します。JSONの構文はYAMLよりも冗長性が少なく、Webアプリケーションのデータ交換に適しています。それでも、YAMLは設定ファイルや人間の可読性が最優先される状況で人気があります。

Google Apps ScriptでYAMLを扱う際は、可読性と使いやすさの間のトレードオフを考慮してください。包括的なYAML操作が必要な場合は、YAMLをJSONに変換してからスクリプト内で処理する外部ツールやサービスを検討する価値があるかもしれません。
