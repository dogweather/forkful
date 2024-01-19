---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ？
部分文字列の抽出は、大きな文字列から特定の部分を取り出すプロセスです。日々のプログラミング作業では、この技術が必要となるケースが多々あります。例えば、ユーザーからの入力を解析したり、特定の情報を検索したりする際に役立ちます。

## どのように？
PHPで文字列を抽出するのは簡単です。それには `substr` と `mb_substr`関数を使用します。例えば：

```PHP
<?php
$str = "こんにちは、世界！";
 
// 部分文字列の抽出
echo substr($str, 0, 9);
?>
```

出力：

```PHP
こんにちは
```

また、日本語（マルチバイト文字列）の操作には `mb_` 関数を使用します。

```PHP
<?php
$str = "こんにちは、世界！";
 
// 部分文字列の抽出
echo mb_substr($str, 0, 5);
?>
```

出力：

```PHP
こんにちは
```

## ディープダイブ
部分文字列の抽出は、古くから存在するプログラミングの基本技術です。初期のプログラミング言語にも部分文字列の抽出を行う機能は含まれていました。

PHPでも `substr` と `mb_substr` 以外にも、正規表現を使用した `preg_match` や `preg_match_all` を使用することで、より高度な部分文字列の抽出が可能です。

'## 参照先
- [PHP公式ドキュメンテーション: substr関数](https://www.php.net/manual/ja/function.substr.php)
- [PHP公式ドキュメンテーション: mb_substr関数](https://www.php.net/manual/ja/function.mb-substr.php)
- [PHP公式ドキュメンテーション: preg_match関数](https://www.php.net/manual/ja/function.preg-match.php)