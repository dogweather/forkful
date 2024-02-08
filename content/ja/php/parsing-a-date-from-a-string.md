---
title:                "文字列から日付をパースする"
date:                  2024-02-03T19:15:10.957514-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から日付をパースする"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
