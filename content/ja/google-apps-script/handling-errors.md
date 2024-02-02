---
title:                "エラーの処理"
date:                  2024-02-01T21:55:23.766784-07:00
model:                 gpt-4-0125-preview
simple_title:         "エラーの処理"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/handling-errors.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Google Apps Scriptでのエラーハンドリングとは、スクリプトの実行中に発生する例外やエラーを予測、キャッチ、そして対応することです。プログラマーは、予期せぬ障害に対してスクリプトを保護し、エラーを上手く管理またはログに記録することができる、よりスムーズでユーザーフレンドリーなアプリケーションを確実にするためにこれを実装します。

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