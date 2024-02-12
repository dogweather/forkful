---
title:                "デバッグ出力の印刷"
date:                  2024-02-01T21:57:57.816152-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッグ出力の印刷"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/printing-debug-output.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
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
