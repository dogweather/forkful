---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字のパターンに一致する文字を削除するとは、特定のパターン（例:数字、特殊文字など）に一致する全ての文字をプログラムから削除する操作です。これは、データクレンジング、フォーマット統一、ユーザー入力の検証など、多くのタスクを簡易化するためにプログラマーが行います。

## 実践方法:

以下のコードは `preg_replace` 関数を用いて、「Hello, World! 123」から全ての数字を削除する例です:

```PHP
<?php
$text = 'Hello, World! 123';
// パターンマッチで数字を削除
$result = preg_replace('/[0-9]/', '', $text);
echo $result;  // 出力: Hello, World! 
?>
```

## ディープダイブ:

正規表現は1980年代初頭にUnix対話シェルと編集器の中で初めて利用され、その後、文字のマッチング、検索、置換などのタスクを実行するためのパワフルなツールとして広く受け入れられました。

PHPで文字パターンを削除する他の方法としての `str_replace` がありますが、これは固定文字列のみに使用可能で、正規表現パターンは使用できません。

また、`preg_replace` の実装はPerl互換正規表現ライブラリであるPCREライブラリに依存しています。これは非常に効率的で強力なライブラリであり、文字列操作のための複雑な正規表現パターンを使用することが可能です。

## 参考リンク:

- PHP 公式ドキュメンテーション（正規表現）: [ここ](https://www.php.net/manual/ja/book.pcre.php)をクリック
- PCREライブラリの詳細: [ここ](https://www.pcre.org/original/doc/html/pcre.html)をクリック
- `preg_replace` 関数の詳細と使い方: [ここ](https://www.php.net/manual/ja/function.preg-replace.php)をクリック
- `str_replace` 関数の詳細と使い方: [ここ](https://www.php.net/manual/ja/function.str-replace.php)をクリック