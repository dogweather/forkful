---
title:                "PHP: 「サブストリングの抽出」"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ？
抽出するためにサブ文字列を使用する理由は多岐にわたります。一般的な用途として、テキストから特定の部分を取り出したい場合や、文字列の比較や検索を行いたい場合などが挙げられます。

## 方法
```php
// 文字列から特定の部分を抽出する例
$str = "こんにちは、私の名前は太郎です。";
$substr = substr($str, 4, 6); // "名前は太郎"
echo $substr;
```

```php
// 文字列を比較する例
$str1 = "apple";
$str2 = "banana";
$substr = substr($str1, 0, 3); // "app"
if($substr == $str2) {
  echo "同じ文字列です";
} else {
  echo "違う文字列です";
}
```

抽出したい部分の始まりの位置と、その長さを指定することで、サブ文字列を取得することができます。

## ディープダイブ
サブ文字列を抽出する際、重要な点は「始まりの位置」と「長さ」を正しく指定することです。始まりの位置は、文字列の先頭を0とし、その後に順番に1, 2, 3...と数を振っていきます。また、長さが指定されない場合は、文字列の最後までを抽出します。

## See Also
- [PHP: substr](https://www.php.net/manual/en/function.substr.php)
- [PHP: String Functions](https://www.php.net/manual/en/ref.strings.php)
- [How to extract a substring in PHP](https://stackoverflow.com/questions/4468268/how-to-extract-a-substring-in-php)