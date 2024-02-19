---
aliases:
- /ja/google-apps-script/getting-the-current-date/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:09.340930-07:00
description: "Google Apps\u2026"
lastmod: 2024-02-18 23:08:54.533356
model: gpt-4-0125-preview
summary: "Google Apps\u2026"
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
---

{{< edit_this_page >}}

## 何となぜ？

Google Apps Scriptで現在の日付を取得することは、ライブの日付と時間を取得するための一般的な作業であり、Googleのエコシステムに関連するアプリでの自動化タスク、ログ取り、タイムスタンプ付けに使用されます。プログラマーは、Googleドキュメント、シート、およびその他のGoogleサービス内での動的コンテンツの生成、締め切りの追跡、スケジューリングにこれを使用します。

## 方法：

JavaScriptに基づいているGoogle Apps Scriptは、現在の日付を取得するためのストレートフォワードな方法を提供します。`new Date()`コンストラクタを使用して、現在の日付と時間を表す新しい日付オブジェクトを作成できます。このオブジェクトをさまざまな形式で操作して表示する方法は次のとおりです。

```javascript
function showCurrentDate() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // スクリプトのタイムゾーンで現在の日付と時間をログに記録
  
  // 日付をYYYY-MM-DD形式で表示する
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // 例: "2023-04-01"
  
  // より読みやすい形式で表示する
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // 例: "April 1, 2023, 12:00:00 PM GMT+1"
}
```

これらのスニペットは、現在の日付と時間をキャプチャしてフォーマットする方法を示しており、Google Apps Script内のさまざまなプログラミングのニーズに対する多様性を示しています。

## 詳細への深掘り

JavaScriptが`Date`オブジェクトに落ち着く前は、プログラマーはタイムスタンプの整数や自家製の日付関数の使用など、標準化されていないよりも煩雑な手段を通して時間と日付を手動で追跡しなければなりませんでした。これは、プログラミング環境によって異なり、一貫性と互換性の問題につながりました。

JavaScript、そして拡張としてのGoogle Apps Scriptでの`new Date()`オブジェクトの導入は、日付と時間の操作を標準化し、日付関連の操作に必要なコードの量を減らし、直感的で扱いやすくしました。Google Apps Scriptの実装が便利であり、Googleの製品スイート内の多くのアプリケーションにとって十分であることに注意が必要ですが、特に複雑なタイムゾーンの処理や高速環境での正確なタイムスタンプログが必要なシナリオには対応していない場合があります。

そのような高度な使用例のために、プログラマーはしばしばMoment.jsやdate-fnsのようなJavaScriptのライブラリに頼ります。Google Apps Scriptはこれらのライブラリをネイティブにサポートしていませんが、開発者は利用可能なJavaScript Dateメソッドを使用して、またはHTMLサービスやApps ScriptのURLフェッチサービスを通じて外部ライブラリにアクセスすることで、これらの機能性のいくつかを模倣することができます。これらの代替にもかかわらず、Google Apps Scriptのネイティブな日付と時刻の機能の単純さと統合は、ほとんどのGoogleエコシステムタスクにおいて依然として最初の選択肢となっています。
