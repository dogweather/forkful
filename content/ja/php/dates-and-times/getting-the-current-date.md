---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:27.589459-07:00
description: "PHP\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\
  \u3053\u3068\u306F\u3001\u30B7\u30B9\u30C6\u30E0\u306E\u65E5\u6642\u3092\u53D6\u5F97\
  \u304A\u3088\u3073\u64CD\u4F5C\u3059\u308B\u305F\u3081\u306E\u57FA\u672C\u7684\u306A\
  \u30BF\u30B9\u30AF\u3067\u3059\u3002\u3053\u308C\u306F\u3001\u30ED\u30B0\u8A18\u9332\
  \u3001\u6295\u7A3F\u306E\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\u3001\u30A4\u30D9\
  \u30F3\u30C8\u306E\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u3001\u307E\u305F\
  \u306F\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u3067\u6642\u9593\u306B\
  \u654F\u611F\u306A\u64CD\u4F5C\u3092\u5B9F\u884C\u3059\u308B\u305F\u3081\u306A\u3069\
  \u306E\u6A5F\u80FD\u306B\u3068\u3063\u3066\u91CD\u8981\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:42.261451-06:00'
model: gpt-4-0125-preview
summary: "PHP\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u3053\
  \u3068\u306F\u3001\u30B7\u30B9\u30C6\u30E0\u306E\u65E5\u6642\u3092\u53D6\u5F97\u304A\
  \u3088\u3073\u64CD\u4F5C\u3059\u308B\u305F\u3081\u306E\u57FA\u672C\u7684\u306A\u30BF\
  \u30B9\u30AF\u3067\u3059\u3002\u3053\u308C\u306F\u3001\u30ED\u30B0\u8A18\u9332\u3001\
  \u6295\u7A3F\u306E\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\u3001\u30A4\u30D9\u30F3\
  \u30C8\u306E\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u3001\u307E\u305F\u306F\
  \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u3067\u6642\u9593\u306B\u654F\
  \u611F\u306A\u64CD\u4F5C\u3092\u5B9F\u884C\u3059\u308B\u305F\u3081\u306A\u3069\u306E\
  \u6A5F\u80FD\u306B\u3068\u3063\u3066\u91CD\u8981\u3067\u3059\u3002"
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 何となぜ？
PHPで現在の日付を取得することは、システムの日時を取得および操作するための基本的なタスクです。これは、ログ記録、投稿のタイムスタンプ、イベントのスケジューリング、またはアプリケーション内で時間に敏感な操作を実行するためなどの機能にとって重要です。

## 方法:
### ネイティブPHP
PHPの組み込みの`date()`関数は、現在の日付を取得する最も直接的な方法です。書式パラメータを指定することによって、さまざまな方法で日付をフォーマットすることができます。

```php
echo date("Y-m-d"); // 出力例: 2023-04-01
echo date("l, F j, Y"); // 出力例: Saturday, April 1, 2023
```

タイムゾーンをサポートした日付と時刻を取得するには、`DateTime`クラスと`DateTimeZone`を一緒に使用します。

```php
$dateTime = new DateTime('now', new DateTimeZone('America/New_York'));
echo $dateTime->format('Y-m-d H:i:s'); // 出力例: 2023-04-01 12:00:00
```

### Carbon（人気のあるサードパーティ製ライブラリ）を使用
[Carbon](https://carbon.nesbot.com/)は、`DateTime`に対するシンプルなAPI拡張であり、日付と時刻を扱うためのよりクリーンで流暢な方法を提供します。

まず、Composer経由でCarbonがインストールされていることを確認してください：
```bash
composer require nesbot/carbon
```

その後、現在の日付を取得するためにそれを使用できます：

```php
use Carbon\Carbon;

echo Carbon::now(); // 出力例: 2023-04-01 12:00:00 (デフォルトフォーマットでの例)
echo Carbon::now()->toDateString(); // 出力例: 2023-04-01
echo Carbon::now()->format('l, F j, Y'); // 出力例: Saturday, April 1, 2023
```

Carbonは、PHPでの日付時間操作を読みやすくし、時間の操作、比較、フォーマットに関する機能の豊富な機能を加えて強化します。
