---
aliases:
- /ja/google-apps-script/converting-a-date-into-a-string/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:13.320117-07:00
description: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3053\
  \u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u65E5\u4ED8\u60C5\u5831\
  \u3092\u4EBA\u9593\u304C\u8AAD\u3081\u308B\u5F62\u5F0F\u3067\u64CD\u4F5C\u304A\u3088\
  \u3073\u8868\u793A\u3067\u304D\u308B\u3088\u3046\u306B\u3059\u308B\u57FA\u672C\u7684\
  \u306A\u30BF\u30B9\u30AF\u3067\u3059\u3002\u3053\u308C\u306F\u3001\u30E6\u30FC\u30B6\
  \u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u306E\u4F5C\u6210\u3001\u30EC\
  \u30DD\u30FC\u30C8\u306E\u751F\u6210\u3001\u307E\u305F\u306FGoogle Apps Script\u3067\
  \u958B\u767A\u3055\u308C\u305F\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\
  \u306E\u60C5\u5831\u306E\u8A18\u9332\u306A\u3069\u306B\u4E0D\u53EF\u6B20\u3067\u3059\
  \u3002"
lastmod: 2024-02-18 23:08:54.534255
model: gpt-4-0125-preview
summary: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3053\
  \u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u65E5\u4ED8\u60C5\u5831\
  \u3092\u4EBA\u9593\u304C\u8AAD\u3081\u308B\u5F62\u5F0F\u3067\u64CD\u4F5C\u304A\u3088\
  \u3073\u8868\u793A\u3067\u304D\u308B\u3088\u3046\u306B\u3059\u308B\u57FA\u672C\u7684\
  \u306A\u30BF\u30B9\u30AF\u3067\u3059\u3002\u3053\u308C\u306F\u3001\u30E6\u30FC\u30B6\
  \u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u306E\u4F5C\u6210\u3001\u30EC\
  \u30DD\u30FC\u30C8\u306E\u751F\u6210\u3001\u307E\u305F\u306FGoogle Apps Script\u3067\
  \u958B\u767A\u3055\u308C\u305F\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\
  \u306E\u60C5\u5831\u306E\u8A18\u9332\u306A\u3069\u306B\u4E0D\u53EF\u6B20\u3067\u3059\
  \u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

日付を文字列に変換することは、プログラマーが日付情報を人間が読める形式で操作および表示できるようにする基本的なタスクです。これは、ユーザーインターフェースの作成、レポートの生成、またはGoogle Apps Scriptで開発されたアプリケーションでの情報の記録などに不可欠です。

## 方法:

Google Apps ScriptはJavaScriptに基づいているため、日付を文字列に変換するために複数の方法を提供しています。以下は、異なるアプローチを示す例です：

### `toString()` メソッドを使用:
最も直接的な方法は、`toString()` メソッドを使用することで、これにより日付オブジェクトがデフォルト形式の文字列に変換されます。

```javascript
var date = new Date();  // 新しい日付オブジェクトを作成
var dateString = date.toString();
Logger.log(dateString); // 出力: "Wed Apr 05 2023 12:34:56 GMT-0700 (Pacific Daylight Time)"
```

### `toDateString()` メソッドを使用:
時間情報なしで読みやすい形式で日付部分のみを取得するには、`toDateString()` を使用できます。

```javascript
var date = new Date();
var dateString = date.toDateString();
Logger.log(dateString); // 出力: "Wed Apr 05 2023"
```

### カスタム形式のために `Utilities.formatDate()` を使用:
形式をより詳細に制御するには、Google Apps Scriptが提供する `Utilities.formatDate()` を使用します。この方法には、日付オブジェクト、タイムゾーン、および形式文字列の3つのパラメーターが必要です。

```javascript
var date = new Date();
var timeZone = Session.getScriptTimeZone();
var formattedDate = Utilities.formatDate(date, timeZone, "YYYY-MM-dd");
Logger.log(formattedDate); // 出力: "2023-04-05"
```

この方法は、ロケール固有、または特定のアプリケーション要件に適した形式で日付を生成するために特に強力です。

## 深掘り

Google Apps Scriptにおける日付を文字列に変換する必要性は、すべてのプログラミング言語に共通するものです。しかし、JavaScriptから継承されたGoogle Apps Scriptのアプローチは、ウェブベースのスクリプティングに向けた柔軟なオプションセットを提供します。`Utilities.formatDate()`は、しばしば見過ごされがちなタイムゾーンでの作業の複雑さを認識する点で際立っています。

歴史的に、日付と時刻の処理は、ソフトウェア開発においてバグや複雑さの源泉でした。これは主に、タイムゾーンや形式の違いによるものです。Google Apps Scriptにおける`Utilities.formatDate()`の導入は、特にグローバルに使用されるGoogleの製品群のコンテキストにおいて、日付時間の操作を標準化することへの言及です。

しかし、特に国際化されたアプリケーションにおいて、タイムゾーン、ロケール、および形式を細かく制御することが必要な場合、開発者は`Moment.js`（バンドルサイズの懸念および現代的な機能のために`Luxon`、`Day.js`、および`date-fns`への好みが高まっているにも関わらず）などの外部ライブラリーを利用することがあるでしょう。このアプローチは、外部依存関係を追加し、プロジェクトの複雑さを増す可能性があるというトレードオフが伴います。

外部ライブラリーの可能性にもかかわらず、`Utilities.formatDate()`およびネイティブのJavaScript日付メソッドは、一般的なユースケースのほとんどに対する堅牢な解決策を提供します。熟練した開発者は、プロジェクトの特定のニーズに応じて、組み込み関数のシンプルさと利便性と、外部ライブラリのパワーと柔軟性をバランスさせるでしょう。
