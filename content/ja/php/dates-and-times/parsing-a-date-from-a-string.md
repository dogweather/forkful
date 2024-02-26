---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:10.957514-07:00
description: "PHP\u3067\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\
  \u30B9\u3059\u308B\u3053\u3068\u306F\u3001\u65E5\u4ED8\u304A\u3088\u3073/\u307E\u305F\
  \u306F\u6642\u9593\u3092\u8868\u3059\u30C6\u30AD\u30B9\u30C8\u3092PHP\u306E`DateTime`\u30AA\
  \u30D6\u30B8\u30A7\u30AF\u30C8\u307E\u305F\u306F\u305D\u306E\u4ED6\u306E\u65E5\u4ED8\
  /\u6642\u9593\u5F62\u5F0F\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3092\u610F\u5473\
  \u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\
  \u3084\u5916\u90E8\u30BD\u30FC\u30B9\u304B\u3089\u306E\u30C7\u30FC\u30BF\u3092\u6271\
  \u3046\u969B\u3001\u7279\u306B\u30C7\u30FC\u30BF\u306E\u691C\u8A3C\u3001\u64CD\u4F5C\
  \u3001\u4FDD\u5B58\u3001\u304A\u3088\u3073\u8868\u793A\u306E\u76EE\u7684\u3067\u975E\
  \u5E38\u306B\u91CD\u8981\u3067\u3059\u3002"
lastmod: '2024-02-25T18:49:40.256763-07:00'
model: gpt-4-0125-preview
summary: "PHP\u3067\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\
  \u3059\u308B\u3053\u3068\u306F\u3001\u65E5\u4ED8\u304A\u3088\u3073/\u307E\u305F\u306F\
  \u6642\u9593\u3092\u8868\u3059\u30C6\u30AD\u30B9\u30C8\u3092PHP\u306E`DateTime`\u30AA\
  \u30D6\u30B8\u30A7\u30AF\u30C8\u307E\u305F\u306F\u305D\u306E\u4ED6\u306E\u65E5\u4ED8\
  /\u6642\u9593\u5F62\u5F0F\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3092\u610F\u5473\
  \u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\
  \u3084\u5916\u90E8\u30BD\u30FC\u30B9\u304B\u3089\u306E\u30C7\u30FC\u30BF\u3092\u6271\
  \u3046\u969B\u3001\u7279\u306B\u30C7\u30FC\u30BF\u306E\u691C\u8A3C\u3001\u64CD\u4F5C\
  \u3001\u4FDD\u5B58\u3001\u304A\u3088\u3073\u8868\u793A\u306E\u76EE\u7684\u3067\u975E\
  \u5E38\u306B\u91CD\u8981\u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
---

{{< edit_this_page >}}

## 何となく理由

PHPで文字列から日付をパースすることは、日付および/または時間を表すテキストをPHPの`DateTime`オブジェクトまたはその他の日付/時間形式に変換することを意味します。これは、ユーザー入力や外部ソースからのデータを扱う際、特にデータの検証、操作、保存、および表示の目的で非常に重要です。

## 方法：

PHPの組み込み`DateTime`クラスは、日付をパースして操作するための強力な機能セットを提供します。コンストラクタを使って日付文字列から`DateTime`インスタンスを作成し、必要に応じてフォーマットできます。以下の方法です：

```php
$dateString = "2023-04-25 15:30:00";
$dateObject = new DateTime($dateString);

echo $dateObject->format('Y-m-d H:i:s');
// 出力：2023-04-25 15:30:00
```

非標準フォーマットの文字列を扱う場合、入力日付の正確なフォーマットを指定できる`createFromFormat`メソッドを使用できます：

```php
$dateString = "25-04-2023 3:30 PM";
$dateObject = DateTime::createFromFormat('d-m-Y g:i A', $dateString);

echo $dateObject->format('Y-m-d H:i:s');
// 出力：2023-04-25 15:30:00
```

`DateTime`によって直接サポートされないようなより複雑なパースを行う場合、PHPは英文の日時記述をUnixタイムスタンプに変換しようとする`strtotime`関数を提供しています：

```php
$timestamp = strtotime("next Thursday");
echo date('Y-m-d', $timestamp);
// 現在の日付によって出力は異なります。例："2023-05-04"
```

**サードパーティのライブラリを使用する：**

PHPの組み込み関数は幅広いユースケースをカバーしていますが、時にはより洗練されたパース機能が必要になることがあります。PHPのDateTimeクラスの拡張であるCarbonライブラリは、日付/時間の操作に関する豊富な機能セットを提供します：

```php
require 'vendor/autoload.php';

use Carbon\Carbon;

$dateString = "Tomorrow";
$date = Carbon::parse($dateString);

echo $date->toDateTimeString();
// 出力は変わります。例："2023-04-26 00:00:00"
```

Carbonの`parse`メソッドは多くの日付および時間の形式を賢く扱えるため、柔軟な日付パース機能が必要なアプリケーションにとって貴重なツールとなります。
