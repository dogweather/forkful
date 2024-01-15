---
title:                "サブストリングの抽出"
html_title:           "PHP: サブストリングの抽出"
simple_title:         "サブストリングの抽出"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

抽出した部分文字列を使用する理由は、必要なデータをより簡単に取得し、データの操作や処理をよりスムーズに行うことができるからです。

## 方法

部分文字列を取得する方法は簡単です。まずは、抽出したい文字列を指定し、その後に開始位置と長さを指定することで、部分文字列を取得することができます。以下のPHPコードを参考にしてください。

```PHP
// 文字列の定義
$text = "今日はいい天気ですね";

// 文字列の抽出
$substring = substr($text, 3, 3);

// 結果の出力
echo $substring;

// 出力結果: はいい
```

この例では、文字列の4文字目から3文字分の部分文字列を取得しています。そして、取得した部分文字列をecho文で出力しています。

## ディープダイブ

部分文字列を取得する際には、使用する関数によって取得する方法が異なります。代表的な関数としては、substr()、mb_substr()、preg_match()などがあります。それぞれの関数の使い方について、以下で詳しく説明していきます。

### substr()

上記のサンプルコードでも使用しているsubstr()関数は、指定した文字列の部分文字列を取得する際に使用します。substr()関数では、第一引数には元の文字列を指定し、第二引数には取得したい部分文字列の開始位置を、第三引数には取得したい部分文字列の長さを指定します。また、第二引数や第三引数を省略することで文字列の最後まで取得することもできます。

```PHP
// 文字列の定義
$text = "今日はいい天気ですね";

// 文字列の抽出
$substring = substr($text, 3, 3);

// 結果の出力
echo $substring;

// 出力結果: はいい
```

### mb_substr()

mb_substr()関数は、マルチバイト文字を扱う際に使用します。マルチバイト文字とは、日本語や中国語などの2バイト以上の文字のことです。このような文字を扱う際には、mb_substr()関数を使用することで文字化けや不具合が起きる可能性を低減することができます。

```PHP
// 文字列の定義
$text = "今日はいい天気ですね";

// 文字列の抽出
$substring = mb_substr($text, 3, 3);

// 結果の出力
echo $substring;

// 出力結果: いい天気
```

### preg_match()

preg_match()関数は、正規表現を使用して特定のパターンに一致する文字列を抽出する際に使用します。正規表現については、深く理解していなくても基本的な使用方法は覚えることができます。

```PHP
// 文字列の定義
$text = "私の電話番号は012-3456-7890です";

// 文字列の抽出
preg_match("/\d{3}-\d{4}-\d{4}/", $text, $matches);

// 結果の出力
echo $matches[0];

// 出力結果: 012-3456-7890
```

## 参考リンク

- [PHP Manual: substr()](https://www.php.net/manual/en/function.substr.php)
- [PHP Manual: mb_substr()](https://www.php.net