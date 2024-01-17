---
title:                "パターンにマッチする文字を削除する"
html_title:           "PHP: パターンにマッチする文字を削除する"
simple_title:         "パターンにマッチする文字を削除する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## とは何か？
文字列の中から、指定されたパターンと一致する文字を削除することを指します。プログラマーがこれを行う理由は、不要な文字を取り除いてデータを整理するためです。

## 方法：
PHPの特定の構文を使用して、文字パターンに一致する文字を削除することができます。例えば、次のようなコードを使用して、文字列から空白を取り除くことができます。

```
$str = "This is a sample string with spaces.";
echo preg_replace('/\s+/', '', $str);
// Output: Thisisasamplestringwithspaces.
```

## 詳細
- 削除する文字パターンは、正規表現と呼ばれるものを使用して指定します。
- 正規表現は、文字列を比較するための柔軟な方法です。
- 他の方法として、文字列全体を処理する場合は、str_replace()関数を使用することもできます。

## 関連リンク
- PHPのpreg_replace関数のドキュメンテーション: https://www.php.net/manual/en/function.preg-replace.php
- 正規表現の学習サイト: https://www.regular-expressions.info/