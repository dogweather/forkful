---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:16:04.551640-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (なぜか？)
PHPで「現在の日付」を取得することは、情報を日付でマークする基本的な作業です。ログ、レポート、ユーザーの活動時間などを追跡するためによく使われます。

## How to: (方法)
PHPの`date`関数を使って、現在の日付や時刻を取得します。簡単な例を見てみましょう。

```PHP
<?php
echo date("Y-m-d H:i:s"); // 年-月-日 時:分:秒の形式
?>
```

実行例：

```
2023-03-15 14:30:25
```

日付のフォーマットは変更可能で、必要に応じてカスタマイズできます。

## Deep Dive (詳細分析)
`date`関数はPHP 4から導入されていますが、PHP 5.2以降のバージョンでは、タイムゾーンの設定が必要です。これには、`date_default_timezone_set`関数が使われます。 

また、`DateTime` クラスもあり、オブジェクト指向に基づいた日付の取得や操作ができます。例えば：

```PHP
<?php
$datetime = new DateTime();
echo $datetime->format('Y-m-d H:i:s');
?>
```

実行結果は `date` 関数と同じですが、`DateTime` クラスのほうが柔軟で拡張性もあります。

## See Also (関連情報)
- PHPの公式ドキュメント: [date](https://www.php.net/manual/ja/function.date.php)
- PHPの公式ドキュメント: [DateTime](https://www.php.net/manual/ja/class.datetime.php)
- タイムゾーンの設定について: [date_default_timezone_set](https://www.php.net/manual/ja/function.date-default-timezone-set.php)