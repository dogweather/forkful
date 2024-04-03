---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:23.766784-07:00
description: "Google Apps\u2026"
lastmod: '2024-03-13T22:44:41.455804-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u3067\u306E\u30A8\u30E9\u30FC\u30CF\u30F3\u30C9\u30EA\
  \u30F3\u30B0\u3068\u306F\u3001\u30B9\u30AF\u30EA\u30D7\u30C8\u306E\u5B9F\u884C\u4E2D\
  \u306B\u767A\u751F\u3059\u308B\u4F8B\u5916\u3084\u30A8\u30E9\u30FC\u3092\u4E88\u6E2C\
  \u3001\u30AD\u30E3\u30C3\u30C1\u3001\u305D\u3057\u3066\u5BFE\u5FDC\u3059\u308B\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4E88\u671F\
  \u305B\u306C\u969C\u5BB3\u306B\u5BFE\u3057\u3066\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\
  \u4FDD\u8B77\u3057\u3001\u30A8\u30E9\u30FC\u3092\u4E0A\u624B\u304F\u7BA1\u7406\u307E\
  \u305F\u306F\u30ED\u30B0\u306B\u8A18\u9332\u3059\u308B\u3053\u3068\u304C\u3067\u304D\
  \u308B\u3001\u3088\u308A\u30B9\u30E0\u30FC\u30BA\u3067\u30E6\u30FC\u30B6\u30FC\u30D5\
  \u30EC\u30F3\u30C9\u30EA\u30FC\u306A\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\
  \u3092\u78BA\u5B9F\u306B\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u5B9F\u88C5\
  \u3057\u307E\u3059\u3002."
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
