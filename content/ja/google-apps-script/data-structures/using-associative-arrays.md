---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:18.248544-07:00
description: "\u9023\u60F3\u914D\u5217\u306F\u3001Google Apps\u2026"
lastmod: '2024-03-13T22:44:41.434069-06:00'
model: gpt-4-0125-preview
summary: "\u9023\u60F3\u914D\u5217\u306F\u3001Google Apps Script\uFF08JavaScript\u306E\
  \u30D0\u30EA\u30A2\u30F3\u30C8\uFF09\u3067\u306F\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\
  \u3068\u3057\u3066\u77E5\u3089\u308C\u3066\u304A\u308A\u3001\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u304C\u30AD\u30FC\u3068\u5024\u306E\u30DA\u30A2\u306E\u30B3\u30EC\u30AF\
  \u30B7\u30E7\u30F3\u3092\u4F5C\u6210\u3059\u308B\u3053\u3068\u3092\u53EF\u80FD\u306B\
  \u3057\u307E\u3059\u3002\u3053\u306E\u6A5F\u80FD\u306F\u7279\u306B\u3001\u52D5\u7684\
  \u306B\u540D\u4ED8\u3051\u3089\u308C\u305F\u30D7\u30ED\u30D1\u30C6\u30A3\u3092\u6271\
  \u3046\u5834\u5408\u3084\u3001\u5F93\u6765\u306E\u914D\u5217\u306E\u7DDA\u5F62\u30B9\
  \u30C8\u30EC\u30FC\u30B8\u3084\u30A2\u30AF\u30BB\u30B9\u30E2\u30C7\u30EB\u304C\u4E0D\
  \u5341\u5206\u306A\u5834\u5408\u306B\u3001\u30C7\u30FC\u30BF\u3092\u52B9\u7387\u7684\
  \u306B\u683C\u7D0D\u304A\u3088\u3073\u64CD\u4F5C\u3059\u308B\u305F\u3081\u306B\u91CD\
  \u8981\u3067\u3059\u3002."
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

## 方法：
Google Apps Scriptで連想配列（オブジェクト）を作成および操作するには、中カッコ `{}` を使用してキーバリューペアを定義します。キーは一意の識別子であり、値は文字列や数値から、より複雑なオブジェクトや関数まで何でもかまいません。基本的な例を以下に示します：

```javascript
function createAssociativeArray() {
  var user = {
    name: "John Doe",
    age: 30,
    email: "johndoe@example.com"
  };

  // 値へのアクセス
  Logger.log(user.name); // 出力: John Doe
  Logger.log(user["email"]); // 出力: johndoe@example.com

  // 新しいキーバリューペアの追加
  user.title = "Software Developer";
  user["country"] = "USA";

  Logger.log(user.title); // 出力: Software Developer

  // キーバリューペアの反復処理
  for (var key in user) {
    Logger.log(key + ': ' + user[key]);
  }
}
```

反復部分のサンプル出力は次のようになるかもしれません：
```
name: John Doe
age: 30
email: johndoe@example.com
title: Software Developer
country: USA
```

プロパティへのアクセスや設定にドット表記やブラケット表記の両方を使用できることに注目してください。ブラケット表記は、特にキーが動的に決定されたものや識別子で許可されていない文字を含む場合に便利です。

## 深掘り
オブジェクトの形式での連想配列は、JavaScriptのプロトタイプベースの継承メカニズムを反映しており、JavaScriptおよび拡張としてのGoogle Apps Scriptの基石となっています。従来の連想配列や辞書（例えばPythonのdict）を持つ言語とは異なり、Google Apps Scriptのオブジェクトは、JavaScriptの動的な性質から得られる柔軟で強力な手段でデータを構造化することを可能にします。

ただし、ECMAScript 2015の仕様が`Map`および`Set`オブジェクトを導入し、挿入順の維持や大規模なデータセットに対するパフォーマンスの向上など、オブジェクトに比べて一部の利点を持つより直接的な連想コレクションの取り扱いを提供したことは重要です。Google Apps Scriptもこれらをサポートしていますが、オブジェクトを使用するか、新しい`Map`/`Set`構造を使用するかの選択は、特定のニーズとパフォーマンスの考慮事項に依存します。ほとんどの連想配列のタスクには、従来のオブジェクトベースの実装が親しみやすく多用途なアプローチを提供しますが、スクリプトの複雑さが増すにつれて、新しい代替手段を検討することが望ましいです。
