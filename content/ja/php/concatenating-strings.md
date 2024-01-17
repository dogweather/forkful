---
title:                "文字列の連結"
html_title:           "PHP: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となんで？
PHPのプログラマーであるあなたは、文字列を連結することはご存知でしょうか？文字列の連結とは、文字列同士を結合して一つの文字列にすることを言います。プログラマーは、主にデータや文言を整形するために文字列を連結します。

## 方法：
以下のようなコードを使って、文字列を連結することができます。PHPの組み込み関数である```concat()```を使用するか、```.```を使って文字列を連結することができます。

```PHP
// 文字列の連結
$name = "山田";
$surname = "太郎";
$name_surname = concat($name, $surname);
echo $name_surname; // 出力結果： 山田太郎

// .を使った文字列の連結
$message = "私の名前は" . $name_surname . "です。";
echo $message; // 出力結果： 私の名前は山田太郎です。
```

## 深堀り：
文字列の連結は、PHPが登場する前から存在するプログラミングの基本的な構文の一つです。PHP以外にも、PythonやJavaScriptでも同様の方法で文字列を連結することができます。また、PHPでは単純に文字列を連結するだけでなく、条件によって変数を含めることもできます。

## 関連情報：
- [PHPのConcat()関数のドキュメント](https://www.php.net/manual/en/function.concat.php)
- [Pythonでの文字列の連結方法](https://www.programiz.com/python-programming/methods/string/join)
- [JavaScriptでの文字列の連結方法](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)