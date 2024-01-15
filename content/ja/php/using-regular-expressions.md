---
title:                "正規表現を使用する"
html_title:           "PHP: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ
なぜ正規表現を使用するのか？それは、テキストのパターンマッチングや置換を行うために、より効率的にコーディングすることができるからです。

## 手順
正規表現を使用するには、まず `preg_match()` 関数を使用してパターンを定義し、それにマッチするテキストを検索します。例えば、`"/[0-9]+/"`という正規表現パターンは、数字を含む文字列を検索します。コードブロック内にPHPのサンプルコードと出力例を記載します。

```PHP
// 正規表現パターンを定義
$pattern = "/[0-9]+/";

// テキストを検索し、数字を含むパターンを出力
$text = "This is a string with numbers like 1234.";
preg_match($pattern, $text, $matches);
print_r($matches);
```

出力結果は以下のようになります。

```
Array
(
    [0] => 1234
)
```

## 深堀り
正規表現は、単純な文字列の検索だけでなく、パターンに基づいた複雑な操作を行うことも可能です。例えば、メールアドレスのバリデーションやHTMLタグの除去などができます。正規表現の詳細な構文や特殊文字をマスターすることで、より高度な操作が可能になります。

## 併せて参考にしてほしい
- [PHPの正規表現マニュアル](https://www.php.net/manual/ja/reference.pcre.pattern.syntax.php)
- [正規表現チュートリアル](https://www.regular-expressions.info/tutorial.html)
- [正規表現を理解するための練習問題](https://regexone.com/)