---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:57.816152-07:00
description: "\u65B9\u6CD5\uFF1A Google Apps Script\u306F\u57FA\u672C\u7684\u306A\u30C7\
  \u30D0\u30C3\u30B0\u306E\u305F\u3081\u306B`Logger`\u30AF\u30E9\u30B9\u3092\u63D0\
  \u4F9B\u3057\u3001\u3088\u308A\u9AD8\u5EA6\u306A\u30CB\u30FC\u30BA\u306B\u5BFE\u3057\
  \u3066\u306F\u3001V8\u30E9\u30F3\u30BF\u30A4\u30E0\u3067\u5C0E\u5165\u3055\u308C\
  \u305F`console`\u30AF\u30E9\u30B9\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002 **Logger\u306E\
  \u4F7F\u7528\uFF1A** Logger\u30AF\u30E9\u30B9\u3092\u5229\u7528\u3059\u308B\u3068\
  \u3001\u5B9F\u884C\u5F8C\u306BApps Script\u30A8\u30C7\u30A3\u30BF\u306E`\u8868\u793A\
  \ >\u2026"
lastmod: '2024-04-05T22:37:49.780803-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Google Apps Script\u306F\u57FA\u672C\u7684\u306A\u30C7\
  \u30D0\u30C3\u30B0\u306E\u305F\u3081\u306B`Logger`\u30AF\u30E9\u30B9\u3092\u63D0\
  \u4F9B\u3057\u3001\u3088\u308A\u9AD8\u5EA6\u306A\u30CB\u30FC\u30BA\u306B\u5BFE\u3057\
  \u3066\u306F\u3001V8\u30E9\u30F3\u30BF\u30A4\u30E0\u3067\u5C0E\u5165\u3055\u308C\
  \u305F`console`\u30AF\u30E9\u30B9\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002 **Logger\u306E\
  \u4F7F\u7528\uFF1A** Logger\u30AF\u30E9\u30B9\u3092\u5229\u7528\u3059\u308B\u3068\
  \u3001\u5B9F\u884C\u5F8C\u306BApps Script\u30A8\u30C7\u30A3\u30BF\u306E`\u8868\u793A\
  \ > \u30ED\u30B0`\u306E\u4E0B\u3067\u78BA\u8A8D\u3067\u304D\u308B\u30C7\u30D0\u30C3\
  \u30B0\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u30ED\u30B0\u306B\u8A18\u9332\u3067\u304D\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306F\u7C21\u5358\u306A\u4F8B\u3067\u3059\uFF1A."
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306E\u5370\u5237"
weight: 33
---

## 方法：
Google Apps Scriptは基本的なデバッグのために`Logger`クラスを提供し、より高度なニーズに対しては、V8ランタイムで導入された`console`クラスを提供します。

**Loggerの使用：**

Loggerクラスを利用すると、実行後にApps Scriptエディタの`表示 > ログ`の下で確認できるデバッグメッセージをログに記録できます。以下は簡単な例です：

```javascript
function logSample() {
  var name = "Wired Reader";
  Logger.log("Hello, %s!", name);
}
```

`logSample()`を実行した後、ログビューアで「Hello, Wired Reader!」というログを確認できます。

**V8ランタイムを使用したconsole.log：**

V8ランタイムを使用すると、他の言語から来た開発者にとってより馴染みのある構文を`console.log`が提供します：

```javascript
function consoleSample() {
  var status = 'active';
  var count = 150;
  console.log(`Current status: ${status}, Count: ${count}`);
}
```

実行後、`表示 > Stackdriver ログ`にアクセスして出力を閲覧します。これは文字列補間やオブジェクト検査をサポートしており、Google Cloudのログと統合されており、永続的なログと高度なフィルタリング機能を提供しているため、より強力です。

**console.logからの出力サンプル：**

```
現在のステータス: active, 数: 150
```

## 深掘り
当初、`Logger.log`はGoogle Apps Scriptでのデバッグのための主要なツールであり、出力を検査するためのシンプルで直接的な方法を提供していました。しかし、スクリプトがより複雑になりGoogle Cloud Platformサービスと統合されるにつれて、より堅牢なロギングソリューションが必要になることが明らかになりました。

V8ランタイムの導入により、`console.log`が登場しました。これはGoogle Apps Scriptを標準的なJavaScript構文に沿わせるだけでなく、JavaScriptに精通した開発者に言語をよりアクセスしやすくしますが、Google Cloudの強力なロギング機能のインフラストラクチャを活用しています。`console.log`とそのGoogle Cloud Platformとの統合の導入は、Google Apps Script内のデバッグ能力の重要な進化を示しており、開発者にスクリプトの監視とトラブルシューティングに対してよりダイナミックでスケーラブルなアプローチを提供しています。

`Logger.log`は基本的なデバッグニーズと小規模プロジェクトには十分ですが、V8ランタイムを用いた`console.log`はより包括的で将来性のある解決策を提供します。これには、実行セッションを超えてログを保持する能力、Google Cloudコンソール内でのログの検索とフィルタリング、そして現代的なJavaScript開発慣行との全体的な整合性が含まれます。ただし、開発者はこれらのオプションを選択する際に、プロジェクトの複雑さと規模に対して自身のニーズを評価するべきです。
