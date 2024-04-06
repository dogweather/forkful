---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:10.957514-07:00
description: "\u65B9\u6CD5\uFF1A PHP\u306E\u7D44\u307F\u8FBC\u307F`DateTime`\u30AF\
  \u30E9\u30B9\u306F\u3001\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3057\u3066\u64CD\u4F5C\
  \u3059\u308B\u305F\u3081\u306E\u5F37\u529B\u306A\u6A5F\u80FD\u30BB\u30C3\u30C8\u3092\
  \u63D0\u4F9B\u3057\u307E\u3059\u3002\u30B3\u30F3\u30B9\u30C8\u30E9\u30AF\u30BF\u3092\
  \u4F7F\u3063\u3066\u65E5\u4ED8\u6587\u5B57\u5217\u304B\u3089`DateTime`\u30A4\u30F3\
  \u30B9\u30BF\u30F3\u30B9\u3092\u4F5C\u6210\u3057\u3001\u5FC5\u8981\u306B\u5FDC\u3058\
  \u3066\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\
  \u306E\u65B9\u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:38:41.787438-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A PHP\u306E\u7D44\u307F\u8FBC\u307F`DateTime`\u30AF\u30E9\
  \u30B9\u306F\u3001\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3057\u3066\u64CD\u4F5C\u3059\
  \u308B\u305F\u3081\u306E\u5F37\u529B\u306A\u6A5F\u80FD\u30BB\u30C3\u30C8\u3092\u63D0\
  \u4F9B\u3057\u307E\u3059\u3002\u30B3\u30F3\u30B9\u30C8\u30E9\u30AF\u30BF\u3092\u4F7F\
  \u3063\u3066\u65E5\u4ED8\u6587\u5B57\u5217\u304B\u3089`DateTime`\u30A4\u30F3\u30B9\
  \u30BF\u30F3\u30B9\u3092\u4F5C\u6210\u3057\u3001\u5FC5\u8981\u306B\u5FDC\u3058\u3066\
  \u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306E\
  \u65B9\u6CD5\u3067\u3059\uFF1A."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

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
