---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:27.589459-07:00
description: "\u65B9\u6CD5: #."
lastmod: '2024-03-13T22:44:42.261451-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

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
