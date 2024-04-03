---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:23.766784-07:00
description: "\u65B9\u6CD5\uFF1A Google Apps Script\u306FJavaScript\u306B\u57FA\u3065\
  \u3044\u3066\u3044\u308B\u305F\u3081\u3001\u30A8\u30E9\u30FC\u30CF\u30F3\u30C9\u30EA\
  \u30F3\u30B0\u306B\u306F\u5F93\u6765\u306E`try-catch`\u30B9\u30C6\u30FC\u30C8\u30E1\
  \u30F3\u30C8\u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002\u6210\u529F\u307E\u305F\
  \u306F\u30A8\u30E9\u30FC\u306B\u95A2\u308F\u3089\u305A\u30AF\u30EA\u30FC\u30F3\u30A2\
  \u30C3\u30D7\u304C\u5FC5\u8981\u306A\u5834\u5408\u306F`finally`\u3082\u4F7F\u7528\
  \u3067\u304D\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.455804-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u306FJavaScript\u306B\u57FA\u3065\u3044\u3066\u3044\u308B\
  \u305F\u3081\u3001\u30A8\u30E9\u30FC\u30CF\u30F3\u30C9\u30EA\u30F3\u30B0\u306B\u306F\
  \u5F93\u6765\u306E`try-catch`\u30B9\u30C6\u30FC\u30C8\u30E1\u30F3\u30C8\u3092\u4F7F\
  \u7528\u3067\u304D\u307E\u3059\u3002\u6210\u529F\u307E\u305F\u306F\u30A8\u30E9\u30FC\
  \u306B\u95A2\u308F\u3089\u305A\u30AF\u30EA\u30FC\u30F3\u30A2\u30C3\u30D7\u304C\u5FC5\
  \u8981\u306A\u5834\u5408\u306F`finally`\u3082\u4F7F\u7528\u3067\u304D\u307E\u3059\
  ."
title: "\u30A8\u30E9\u30FC\u306E\u51E6\u7406"
weight: 16
---

## 方法：
Google Apps ScriptはJavaScriptに基づいているため、エラーハンドリングには従来の`try-catch`ステートメントを使用できます。成功またはエラーに関わらずクリーンアップが必要な場合は`finally`も使用できます。

```javascript
function myFunction() {
  try {
    // エラーを投げる可能性のあるコード
    var sheet = SpreadsheetApp.getActiveSheet();
    var data = sheet.getRange("A1").getValue();
    if (data === "") {
      throw new Error("セルA1が空です。");
    }
    Logger.log(data);
  } catch (e) {
    // エラーハンドリングコード
    Logger.log("エラー: " + e.message);
  } finally {
    // エラーが発生したかどうかに関わらず実行されるクリーンアップコード
    Logger.log("関数が完了しました。");
  }
}
```

エラーなしのサンプル出力：
```
[セルの値]
関数が完了しました。
```

エラーありのサンプル出力（A1が空の場合を想定）：
```
エラー: セルA1が空です。
関数が完了しました。
```

Google Apps Scriptは、`Error`オブジェクトを使用してカスタムエラーを投げることや、必要に応じて特定のエラータイプをキャッチすることもサポートしています。しかし、高度なエラーカテゴリゼーションの不在は、特定性のためにエラーメッセージに依存することを必須としています。

## ディープダイブ
歴史的に、JavaScript（およびそれに伴うGoogle Apps Script）などのスクリプト言語でのエラーハンドリングは、詳細な例外階層や包括的なデバッグツールなどの機能を提供する一部のコンパイル言語よりも洗練されていないことが多かったです。Google Apps Scriptのモデルは比較的単純で、JavaScriptの`try-catch-finally`パラダイムを利用しています。この単純さは、Googleのエコシステム内で小規模から中規模のアプリケーションを迅速に開発し、展開するための言語の設計と一致していますが、複雑なエラーシナリオに取り組む場合、開発者を制限することがあります。

より複雑なアプリケーションでは、プログラマーはしばしば、Google Apps Scriptのネイティブエラーハンドリングをカスタムログやエラーレポーティングメカニズムで補足します。これには、監査用にGoogleシートにエラーを記述することや、Google Apps ScriptのURLフェッチサービスを通じてエラーの詳細をスクリプト環境外に送信するためのサードパーティログサービスの使用が含まれます。

JavaやC#のような言語と比べて、Google Apps Scriptが組み込みのエラーハンドリングの複雑さや機能面で後れをとるかもしれませんが、Googleサービスとの統合と`try-catch-finally`アプローチの単純さが、開発者にとってGoogleのエコシステム内でタスクを迅速に自動化し、統合を作成するための強力なツールとなります。他の背景を持つ開発者にとっては、複雑なエラーハンドリングパターンをマスターすることではなく、利用可能なものを創造的に活用してスクリプトを堅牢でユーザーフレンドリーにすることに挑戦があるかもしれません。
