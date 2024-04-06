---
date: 2024-01-20 17:39:32.911595-07:00
description: "How to (\u5B9F\u88C5\u65B9\u6CD5) PHP\u3067\u306F\u3001`mb_strtolower()`\u95A2\
  \u6570\u3092\u4F7F\u3063\u3066\u7C21\u5358\u306B\u6587\u5B57\u5217\u3092\u5C0F\u6587\
  \u5B57\u306B\u5909\u63DB\u3067\u304D\u307E\u3059\u3002`mb_` \u306F\u30DE\u30EB\u30C1\
  \u30D0\u30A4\u30C8\u6587\u5B57\u306B\u5BFE\u5FDC\u3057\u3066\u304A\u308A\u3001\u65E5\
  \u672C\u8A9E\u3092\u542B\u3080\u591A\u304F\u306E\u8A00\u8A9E\u3067\u5FC5\u8981\u3067\
  \u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:41.760458-06:00'
model: gpt-4-1106-preview
summary: "How to (\u5B9F\u88C5\u65B9\u6CD5) PHP\u3067\u306F\u3001`mb_strtolower()`\u95A2\
  \u6570\u3092\u4F7F\u3063\u3066\u7C21\u5358\u306B\u6587\u5B57\u5217\u3092\u5C0F\u6587\
  \u5B57\u306B\u5909\u63DB\u3067\u304D\u307E\u3059\u3002`mb_` \u306F\u30DE\u30EB\u30C1\
  \u30D0\u30A4\u30C8\u6587\u5B57\u306B\u5BFE\u5FDC\u3057\u3066\u304A\u308A\u3001\u65E5\
  \u672C\u8A9E\u3092\u542B\u3080\u591A\u304F\u306E\u8A00\u8A9E\u3067\u5FC5\u8981\u3067\
  \u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

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
