---
title:                "PHP: 「文字列を小文字に変換する」"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# なぜ
文字列を小文字に変換することに興味があるかもしれません。例えば、入力として大文字と小文字が混ざった文字列が与えられた場合に、一貫性のある表示や比較をするためには小文字に変換する必要があります。

## 方法
```PHP
<?php
// 文字列を小文字に変換する
$input = "HeLlO, wOrLd!";
$output = strtolower($input);
echo $output; // hello, world!
?>
```

## 詳細
文字列を小文字に変換する方法は様々ありますが、ここではPHPの組み込み関数である`strtolower()`を使用しています。この関数は引数として与えられた文字列を全て小文字に変換し、その結果を返します。変換されるのは英字のみで、数字や記号は変換されません。

また、日本語のように文字間に大文字と小文字の区別がない言語でも、この関数を使うことで全ての文字を小文字に統一することができます。

# See Also
- [PHPのstrtolower()関数について](https://www.php.net/manual/ja/function.strtolower.php)
- [文字列の大文字と小文字を区別しない比較について](http://php.net/manual/ja/function.mb-strtolower.php)