---
title:                "文字列を小文字に変換する"
html_title:           "PHP: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ文字列を小文字に変換する必要があるのか

文字列を小文字に変換することは、コードの柔軟性と可読性を高めるために重要です。一貫性のある形式でデータを処理することができ、ソートや比較などの操作を簡単に行うことができます。

## 方法

文字列を小文字に変換するには、PHPの組み込み関数である strtolower() を使用します。まず、変換したい文字列を引数として指定し、小文字に変換された文字列を返すようにします。

```PHP
<?php
// 変換前の文字列を定義
$text = "HELLO WORLD";
// strtolower()関数で文字列を小文字に変換
$lowercase_text = strtolower($text);
// 変換後の文字列を出力
echo $lowercase_text; // 出力結果: hello world
?>
```

もし、変換する文字列がユーザーからの入力や外部データである場合は、事前に trim() 関数を使って余分な空白を除去してから strtolower() 関数を適用することをお勧めします。

```PHP
<?php
// ユーザーからの入力を受け取る
$input = " HELLO WORLD    ";
// 余分な空白を除去
$input = trim($input);
// strtolower()関数で文字列を小文字に変換
$lowercase_input = strtolower($input);
// 変換後の文字列を出力
echo $lowercase_input; // 出力結果: hello world
?>
```

## 深堀り

PHPでは、英語以外の言語を扱う場合にも文字列を小文字に変換することができます。これは、mb_strtolower() 関数を使用することで実現できます。この関数は多言語対応の文字列操作を行うための拡張モジュールである mbstring が必要です。

```PHP
<?php
// 日本語の文字列を定義
$text = "こんにちは、世界";
// mb_strtolower()関数で文字列を小文字に変換
$lowercase_text = mb_strtolower($text);
// 変換後の文字列を出力
echo $lowercase_text; // 出力結果: こんにちは、世界
?>
```

また、mbstring を使用する場合は、第二引数に文字のエンコーディングを指定することができます。

```PHP
<?php
// 日本語の文字列を定義
$text = "こんにちは、世界";
// mb_strtolower()関数で文字列を小文字に変換
$lowercase_text = mb_strtolower($text, "UTF-8");
// 変換後の文字列を出力
echo $lowercase_text; // 出力結果: こんにちは、世界
?>
```

## おわりに

文字列を小文字に変換する方法を学びました。これはソフトウェア開発において非常に重要なテクニックであるため、ぜひ覚えておいてください。また、mbstring を使用する場合は、インストールされているかどうかを確認してから使用するようにしましょう。

## 関連リンク

- PHP公式ドキュメント: [strtolower()](https://www.php.net/manual/en/function.strtolower.php)
- PHP公式ドキュメント: [mb_strtolower()](https://www.php.net/manual/en/function.mb-strtolower.php)
- PHP公式ドキュメント: [mbstring](https://www.php.net/manual