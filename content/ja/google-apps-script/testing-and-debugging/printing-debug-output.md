---
aliases:
- /ja/google-apps-script/printing-debug-output/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:57.816152-07:00
description: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u5370\u5237\u3059\u308B\u3053\
  \u3068\u306F\u3001\u30E9\u30F3\u30BF\u30A4\u30E0\u4E2D\u306B\u5909\u6570\u306E\u5024\
  \u3001\u5B9F\u884C\u30D5\u30ED\u30FC\u3001\u307E\u305F\u306F\u30E1\u30C3\u30BB\u30FC\
  \u30B8\u30A8\u30E9\u30FC\u3092\u8868\u793A\u3059\u308B\u305F\u3081\u306B\u3001\u30B3\
  \u30FC\u30C9\u5185\u306B\u6226\u7565\u7684\u306B\u30ED\u30B0\u30B9\u30C6\u30FC\u30C8\
  \u30E1\u30F3\u30C8\u3092\u914D\u7F6E\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u5E83\u7BC4\
  \u56F2\u306B\u5229\u7528\u3057\u3066\u3001Google Apps\u2026"
lastmod: 2024-02-18 23:08:54.525592
model: gpt-4-0125-preview
summary: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u5370\u5237\u3059\u308B\u3053\
  \u3068\u306F\u3001\u30E9\u30F3\u30BF\u30A4\u30E0\u4E2D\u306B\u5909\u6570\u306E\u5024\
  \u3001\u5B9F\u884C\u30D5\u30ED\u30FC\u3001\u307E\u305F\u306F\u30E1\u30C3\u30BB\u30FC\
  \u30B8\u30A8\u30E9\u30FC\u3092\u8868\u793A\u3059\u308B\u305F\u3081\u306B\u3001\u30B3\
  \u30FC\u30C9\u5185\u306B\u6226\u7565\u7684\u306B\u30ED\u30B0\u30B9\u30C6\u30FC\u30C8\
  \u30E1\u30F3\u30C8\u3092\u914D\u7F6E\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u5E83\u7BC4\
  \u56F2\u306B\u5229\u7528\u3057\u3066\u3001Google Apps\u2026"
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306E\u5370\u5237"
---

{{< edit_this_page >}}

## 何となぜ？

デバッグ出力を印刷することは、ランタイム中に変数の値、実行フロー、またはメッセージエラーを表示するために、コード内に戦略的にログステートメントを配置することを含みます。プログラマーはこれを広範囲に利用して、Google Apps Scriptアプリケーションの正確さと効率を確保するために、スクリプトの振る舞いを追跡し診断します。

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
