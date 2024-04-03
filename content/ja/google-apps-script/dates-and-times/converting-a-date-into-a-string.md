---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:13.320117-07:00
description: "\u65B9\u6CD5: Google Apps Script\u306FJavaScript\u306B\u57FA\u3065\u3044\
  \u3066\u3044\u308B\u305F\u3081\u3001\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\
  \u63DB\u3059\u308B\u305F\u3081\u306B\u8907\u6570\u306E\u65B9\u6CD5\u3092\u63D0\u4F9B\
  \u3057\u3066\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u7570\u306A\u308B\u30A2\
  \u30D7\u30ED\u30FC\u30C1\u3092\u793A\u3059\u4F8B\u3067\u3059\uFF1A #."
lastmod: '2024-03-13T22:44:41.459686-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u306FJavaScript\u306B\u57FA\u3065\u3044\u3066\u3044\u308B\
  \u305F\u3081\u3001\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\
  \u305F\u3081\u306B\u8907\u6570\u306E\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u7570\u306A\u308B\u30A2\u30D7\u30ED\u30FC\
  \u30C1\u3092\u793A\u3059\u4F8B\u3067\u3059\uFF1A\n\n#."
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

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
