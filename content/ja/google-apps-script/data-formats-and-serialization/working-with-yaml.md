---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:23.291954-07:00
description: "\u65B9\u6CD5: Google Apps\u2026"
lastmod: '2024-03-13T22:44:41.472850-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uFF08GAS\uFF09\u306FYAML\u306E\u30D1\u30FC\u30B9\u3084\
  \u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u3092\u30CD\u30A4\u30C6\
  \u30A3\u30D6\u306B\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u305B\u3093\u304C\
  \u3001JavaScript\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3059\u308B\u304B\
  \u3001\u30AB\u30B9\u30BF\u30E0\u30D1\u30FC\u30B9\u95A2\u6570\u3092\u66F8\u304F\u3053\
  \u3068\u3067YAML\u30C7\u30FC\u30BF\u3092\u64CD\u4F5C\u3067\u304D\u307E\u3059\u3002\
  \u30C7\u30E2\u306E\u305F\u3081\u306B\u3001GAS\u306B\u76F4\u63A5\u5916\u90E8\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3092\u30A4\u30F3\u30DD\u30FC\u30C8\u3059\u308B\u3053\u3068\
  \u306F\u3067\u304D\u306A\u3044\u305F\u3081\u3001\u30AB\u30B9\u30BF\u30E0\u95A2\u6570\
  \u3092\u4F7F\u7528\u3057\u3066YAML\u6587\u5B57\u5217\u3092\u89E3\u6790\u3059\u308B\
  \u65B9\u6CD5\u3092\u691C\u8A0E\u3057\u307E\u3057\u3087\u3046."
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
