---
title:    "PHP: サブストリングの抽出"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## なぜ
人々がサブストリングを抽出することに関わる理由を1〜2文で説明します。

サブストリングを抽出する必要がある主な理由の1つは、文字列から特定の部分を抽出して、より特定の情報を取得することです。例えば、メールアドレスからドメイン名を取得したい場合などがあります。

## 方法

```PHP
<?php
$str = "Hello World";
 
// 文字列全体を出力
echo substr($str, 0) . "\n";
 
// 開始位置を指定して部分文字列を出力
echo substr($str, 6) . "\n";
 
// 開始位置と長さを指定して部分文字列を出力
echo substr($str, 6, 3) . "\n";
 
// マイナスの開始位置を指定して部分文字列を出力
echo substr($str, -5) . "\n";
 
// マイナスの開始位置と長さを指定して部分文字列を出力
echo substr($str, -5, 2) . "\n";
 
// 空の文字列を出力
echo substr($str, 3, 0) . "\n";
 
// 文字列全体を出力
echo substr($str, 0, strlen($str)) . "\n";
```

出力:
```
Hello World
World
Wor
World
Wo

Hello World
Hello World
```

この例では、```substr()```関数を使用して、さまざまな方法で部分文字列を抽出する方法を示しています。第一引数には元の文字列、第二引数には抽出したい部分文字列の開始位置、第三引数には抽出する文字数を指定します。マイナスの開始位置を指定することで、文字列の末尾から数えた位置から抽出することもできます。

## ディープダイブ

サブストリングを抽出するために使用される関数は、```substr()```だけではありません。例えば、```mb_substr()```を使用することでマルチバイト文字列でも正しく部分文字列を抽出することができます。また、正規表現を使用することで、特定のパターンにマッチする部分文字列を抽出することもできます。

サブストリングを抽出する際には、文字列の範囲やバイト数、または正規表現を使用するかどうかを考慮する必要があります。

## 参考リンク

- [PHP: substr - Manual](https://www.php.net/manual/en/function.substr.php)
- [PHP: mb_substr - Manual](https://www.php.net/manual/en/function.mb-substr.php)
- [PHP: preg_match - Manual](https://www.php.net/manual/en/function.preg-match.php)

## 参考リンク

- [PHP: substr - Manual](https://www.php.net/manual/ja/function.substr.php)
- [PHP: mb_substr - Manual](https://www.php.net/manual/ja/function.mb-substr.php)
- [PHP: preg_match - Manual](https://www.php.net/manual/ja/function.preg-match.php)