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

## Why - なぜ

文字列の中から特定のパターンに一致する文字を削除することに関心があるかもしれません。これは、不要な情報を取り除き、データの整理を行う際にとても役立つ技術です。

## How To - 方法

削除する文字のパターンを指定して、`preg_replace()`関数を使用することで簡単に削除を行うことができます。`preg_replace()`関数は、正規表現パターンに一致する文字を空の文字列で置き換えることで文字列を変更します。

以下は、`preg_replace()`関数を使用して指定されたパターンに一致する文字を削除するコード例です。

```PHP
$string = "Hello, World!";
$result = preg_replace("/[aeiou]/i", "", $string);
echo $result; // Output: Hll, Wrld!
```

上記のコードでは、文字列から母音（a, e, i, o, u）を削除しています。`preg_replace()`関数の第一引数には、パターンを表す正規表現を指定し、第二引数には新しい文字列を指定します。`i`オプションを使用することで、大文字と小文字の区別をなくしています。

## Deep Dive - より深く

`preg_replace()`関数は、より複雑な正規表現を使用して、文字の削除や置換を行うことも可能です。例えば、`/[^a-z0-9]/i`という正規表現を使用することで、英数字以外の文字を削除することもできます。

また、`preg_replace()`関数は複数のパターンを指定することで、一度に複数の文字を一括で削除することもできます。例えば、`/[aeiou]/i`と`/[!@#$%^&*()]/`の二つの正規表現を指定することで、母音と特定の記号を一括で削除することができます。

さらに、`preg_replace()`関数は、正規表現を使用することで、複雑なパターンに一致する文字列の置換もできます。このように、正規表現を使用することで、より精緻なテキスト処理を行うことができます。

## See Also - 関連リンク

- preg_replace()関数のドキュメンテーション (https://www.php.net/manual/en/function.preg-replace.php)
- PHPでの正規表現の使い方 (https://qiita.com/mpyw/items/30c4370b56c7890f6a36)
- 正規表現の基礎 (https://www.runoob.com/regexp/regexp-syntax.html)