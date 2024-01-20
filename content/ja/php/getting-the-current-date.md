---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何となぜ?

現在の日付を取得するというのは、今日の日付・時間を取得する操作のことです。これはログの作成、報告のタイムスタンプ、日付制限付きの機能など、一般的に時間に依存するタスクをプログラム内で行うために役立ちます。

## 方法:

以下に、現在の日付を取得するPHPコードの例を示します。これは基本的な実装で、phpという環境下で現在の日付と時間を返します：

```PHP
<?php
    echo date("Y-m-d H:i:s");
?>
```

実行すると、以下のような出力が得られます（出力は実行時の日付・時間に基づいて変化します）：

```
2022-03-01 14:03:45
```

## ディープダイブ:

PHPには古くから `date()` 関数が搭載され、現在の日付や時間を取得するためによく使用されています。この関数は非常に柔軟で、さまざまな形式で日付と時間を表示することが可能です。

しかし、日時処理には他にも方法があります。例えば `DateTime` クラスも使用することができます：

```PHP
<?php
    $date = new DateTime();
    echo $date->format('Y-m-d H:i:s');
?>
```

これが返す出力も上記の `date()` 関数と同じです。

取得する日付のフォーマットは、 `date()` 関数や `DateTime` クラスのformatメソッドの引数によって変えることが可能です。Yは四桁の年、mは月、dは日、Hは時間、iは分、そしてsは秒を表します。

## 参考情報:

以下は、関連するトピックについて更に調査するためのリンクです：

1. PHP `date()` 関数: [https://www.php.net/manual/ja/function.date.php](https://www.php.net/manual/ja/function.date.php)

2. PHP `DateTime` クラス: [https://www.php.net/manual/ja/class.datetime.php](https://www.php.net/manual/ja/class.datetime.php)