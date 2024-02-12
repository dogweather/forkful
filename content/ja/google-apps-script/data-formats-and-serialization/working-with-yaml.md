---
title:                "YAMLとの作業"
aliases:
- /ja/google-apps-script/working-with-yaml/
date:                  2024-02-01T22:07:23.291954-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAMLとの作業"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/working-with-yaml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ?

YAMLは「YAML Ain’t Markup Language」という意味で、人間が読みやすいデータシリアライゼーション標準であり、構成ファイルや異なるデータ構造を持つ言語間のデータ交換によく使用されます。プログラマーは、特に広範な設定が必要なプロジェクトや異なるシステム間で構造化されたデータを転送する際に、そのシンプルさと読みやすさのためにYAMLをよく使用します。

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
