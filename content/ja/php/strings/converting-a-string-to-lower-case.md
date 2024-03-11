---
date: 2024-01-20 17:39:32.911595-07:00
description: "\u5909\u6570\u5185\u306E\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\
  \u5909\u63DB\u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u4E00\u8CAB\
  \u6027\u306E\u3042\u308B\u30C7\u30FC\u30BF\u51E6\u7406\u3084\u5927\u6587\u5B57\u30FB\
  \u5C0F\u6587\u5B57\u3092\u533A\u5225\u3057\u306A\u3044\u691C\u7D22\u306E\u305F\u3081\
  \u306B\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.798134-06:00'
model: gpt-4-1106-preview
summary: "\u5909\u6570\u5185\u306E\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\
  \u5909\u63DB\u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u4E00\u8CAB\
  \u6027\u306E\u3042\u308B\u30C7\u30FC\u30BF\u51E6\u7406\u3084\u5927\u6587\u5B57\u30FB\
  \u5C0F\u6587\u5B57\u3092\u533A\u5225\u3057\u306A\u3044\u691C\u7D22\u306E\u305F\u3081\
  \u306B\u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

変数内の文字列を小文字に変換することを指します。一貫性のあるデータ処理や大文字・小文字を区別しない検索のために行います。

## How to (実装方法)

```php
<?php
$originalString = "こんにちは、WORLD!";
$lowerCaseString = mb_strtolower($originalString);

echo $lowerCaseString; // 出力: こんにちは、world!
?>
```

PHPでは、`mb_strtolower()`関数を使って簡単に文字列を小文字に変換できます。`mb_` はマルチバイト文字に対応しており、日本語を含む多くの言語で必要です。

```php
<?php
$anotherString = "PHP is Fun!";
$lowerCased = strtolower($anotherString);

echo $lowerCased; // 出力: php is fun!
?>
```

英語などのシングルバイト文字のみの場合は、`strtolower()`関数が使用できます。

## Deep Dive (深掘り)

文字列を小文字にする処理は、PHPの初期バージョンから存在します。単純な英語のテキストには`strtolower()`を使っていましたが、多言語対応のために`mb_strtolower()`が生まれました。この関数は、エンコーディングを指定することもでき、様々な文字エンコーディングのテキストに対応しています。

JavaScriptの`toLowerCase()`やPythonの`.lower()`と同様に、PHPも開発者がデータを扱いやすくするための機能を提供しています。実際のところ、データベースの検索やソート、ユーザー入力のバリデーションなど、多岐にわたる場面で小文字変換が役立ちます。

実装の裏側では、PHPは文字のASCIIコードを確認し、大文字から小文字へオフセットすることで変換を行いますが、マルチバイト文字に対しては、より複雑なエンコーディングルールが適用されます。

## See Also (関連情報)

- PHP Manual on `mb_strtolower()`: [https://www.php.net/manual/en/function.mb-strtolower.php](https://www.php.net/manual/en/function.mb-strtolower.php)
- PHP Manual on `strtolower()`: [https://www.php.net/manual/en/function.strtolower.php](https://www.php.net/manual/en/function.strtolower.php)
- Unicodeと文字エンコーディングについて: [https://ja.wikipedia.org/wiki/Unicode](https://ja.wikipedia.org/wiki/Unicode)
- 文字エンコーディング機能を利用したPHPの文字列処理に関する詳細: [https://www.php.net/manual/en/book.mbstring.php](https://www.php.net/manual/en/book.mbstring.php)
