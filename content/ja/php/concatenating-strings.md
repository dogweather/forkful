---
title:    "PHP: 文字列の連結"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

文字列を連結することの意義は、プログラミングにおいて必須のスキルです。文字列を連結することにより、より複雑な文字列データを扱うことができます。また、Web開発やデータ処理など、さまざまな場面で活用できます。

## How To

文字列連結は、PHPで非常に簡単に実現することができます。基本的な構文は以下のようになります。

```
<?php
$string1 = "Hello";
$string2 = "World";

// using the "." operator
$output1 = $string1 . $string2;
echo $output1; // output: HelloWorld

// using the concatenation assignment operator (".=")
$string3 = "こんにちは";
$output2 = $string3 .= "世界";
echo $output2; // output: こんにちは世界
?>
```

## Deep Dive

文字列連結を実現するには、`.`演算子または文字列連結代入演算子(`.=`)を使用します。`.`演算子は、2つの文字列を連結する際に使用し、`.=`演算子は、左辺の文字列に右辺の文字列を連結する際に使用します。

また、配列や変数を連結することも可能です。以下のように「{}」を使用することで、変数や配列を展開することができます。

```
<?php
$name = "太郎";
$age = 20;
$interest = array("プログラミング", "旅行", "スポーツ");

// concatenating variables and strings
$output = "私の名前は{$name}です。年齢は{$age}歳です。";
echo $output; // output: 私の名前は太郎です。年齢は20歳です。

// concatenating arrays
$output = "私の趣味は{$interest[0]}、{$interest[1]}、{$interest[2]}です。";
echo $output; // output: 私の趣味はプログラミング、旅行、スポーツです。
?>
```

## See Also

- [PHPの文字列連結方法 (トラハック)](https://torajirohacks.com/2020/04/11/phpconcat/)
- [PHP について学ぶ (日本語リファレンス)](https://php.net/manual/ja/)
- [文字列の連結方法 (W3Schools 日本語版)](https://www.w3schools.com/php/php_string_concat.asp)