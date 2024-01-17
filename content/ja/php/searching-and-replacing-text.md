---
title:                "テキストの検索と置換"
html_title:           "PHP: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何をしていますか？：
テキストの検索と置換が何かを2〜3文で説明し、プログラマーがその作業を行う理由を説明します。

## 方法：
```PHP
// 文字列の置換
$str = "こんにちは、世界！";
echo str_replace("こんにちは", "Hello", $str); // 結果：Hello、世界！

// 正規表現による置換
$str = "apple123banana456";
echo preg_replace('/\d+/', '', $str); // 結果：applebanana
```

## ディープダイブ：
テキストの検索と置換は、古くからプログラミングの重要な機能の1つです。以前は、文字列の操作には手作業が必要でしたが、テキストの検索と置換を使うことで、より簡単に文字列を操作することができるようになりました。また、正規表現を使うことで、パターンマッチングによる高度な置換が可能になります。代替手段としての```str_replace```関数や、テキストエディターの置換機能などがあります。PHPでは、```preg_replace```、```str_replace```、```substr_replace```などの多くの関数がテキストの検索と置換に使用されます。

## 関連リンク：
- [PHPのstr_replace関数](https://www.php.net/manual/ja/function.str-replace.php)
- [PHPのpreg_replace関数](https://www.php.net/manual/ja/function.preg-replace.php)
- [PHPのsubstr_replace関数](https://www.php.net/manual/ja/function.substr-replace.php)