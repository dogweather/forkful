---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:09.340930-07:00
description: "Google Apps\u2026"
lastmod: '2024-03-13T22:44:41.458284-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\
  \u3059\u308B\u3053\u3068\u306F\u3001\u30E9\u30A4\u30D6\u306E\u65E5\u4ED8\u3068\u6642\
  \u9593\u3092\u53D6\u5F97\u3059\u308B\u305F\u3081\u306E\u4E00\u822C\u7684\u306A\u4F5C\
  \u696D\u3067\u3042\u308A\u3001Google\u306E\u30A8\u30B3\u30B7\u30B9\u30C6\u30E0\u306B\
  \u95A2\u9023\u3059\u308B\u30A2\u30D7\u30EA\u3067\u306E\u81EA\u52D5\u5316\u30BF\u30B9\
  \u30AF\u3001\u30ED\u30B0\u53D6\u308A\u3001\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\
  \u4ED8\u3051\u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001Google\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u3001\u30B7\u30FC\
  \u30C8\u3001\u304A\u3088\u3073\u305D\u306E\u4ED6\u306EGoogle\u30B5\u30FC\u30D3\u30B9\
  \u5185\u3067\u306E\u52D5\u7684\u30B3\u30F3\u30C6\u30F3\u30C4\u306E\u751F\u6210\u3001\
  \u7DE0\u3081\u5207\u308A\u306E\u8FFD\u8DE1\u3001\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\
  \u30F3\u30B0\u306B\u3053\u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

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
