---
title:                "PHP: パターンと一致する文字の削除"
simple_title:         "パターンと一致する文字の削除"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

文字のパターンに一致する文字を削除するプログラミングをする理由は、データの整理や処理の効率化のためです。

## 方法

文字のパターンに一致する文字を削除するには、PHPのstr_replace関数を使用します。下記の例では、「-」を削除するコードを示します。

```PHP
$string = "1-2-3-4";
$modified_string = str_replace("-", "", $string);

echo $modified_string // Output: 1234
```

もし文字列内に複数のパターンがある場合は、配列を使用して一括で置換することもできます。

```PHP
$string = "apple-1, banana-2, orange-3";
$patterns = array("-", ",");
$replacements = array("", "");

$modified_string = str_replace($patterns, $replacements, $string);

echo $modified_string // Output: apple1 banana2 orange3
```

このように、str_replace関数を使用することで、簡単に文字のパターンに一致する文字を削除できます。

## ディープダイブ

文字のパターンに一致する文字を削除する方法は、PHPの他の文字列操作関数を使用することでも実現できます。例えば、preg_replace関数を使用することで、正規表現を使用してより複雑なパターンの文字を削除することもできます。

また、str_replace関数の第3引数として文字列の変数を渡すことで、動的にパターンを指定することもできます。

## See Also

- [PHP: str_replace](https://www.php.net/manual/ja/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/ja/function.preg-replace.php)