---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# PHPによる文字列長の取得 (Find Length of A String in PHP)

## 何となぜ？

文字列の長さを取得するとは、文字列内の文字数を計算することです。これは、入力確認、テキストサイズの制限、あるいは特定の位置の文字を操るプログラムを作成する際に必要になる操作です。

## 実行方法:

PHPで文字列の長さを取得することは非常に簡単です。組み込み関数`strlen()`を使用します。以下に例を示します。

```PHP
<?php
$text = "こんにちは、世界!";
echo strlen($text);
?>
```

このコードを実行すると、`21`が出力されます。

## 詳細な説明:

かつて、`strlen()`関数は内部的に文字列をバイトの配列として扱い、1バイトを1文字としてカウントしていました。しかし多バイト文字（例えば日本語）を正しくカウントできない問題がありました。これに対応するため、現在のPHPでは`mb_strlen()`というマルチバイト対応の関数を使用します。

```PHP
<?php
$text = "こんにちは、世界!";
echo mb_strlen($text, "UTF-8");
?>
```
実行すると、今度は`9`が出力されます。これは日本語の文字数により近い結果です。

文字列の長さを取得する他の方法としては正規表現を使用する方法もありますが、パフォーマンス面から見ると`strlen()`や`mb_strlen()`の方が優れています。

## 参考資料:

より深く学びたい方は次のリンクを参考にしてください:
- PHP公式マニュアルでの`strlen()`の詳細: [here](https://www.php.net/manual/function.strlen.php)
- PHP公式マニュアルでの`mb_strlen()`の詳細: [here](https://www.php.net/manual/function.mb-strlen.php)
- Regular Expressions (RegEx) in PHP: [here](https://www.php.net/manual/function.preg-match.php)