---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:04.955400-07:00
description: "\u65B9\u6CD5\uFF1A PHP\u306F\u6A19\u6E96\u3067\u3001\u6587\u5B57\u5217\
  \u3092\u5927\u6587\u5B57\u306B\u3059\u308B\u305F\u3081\u306E\u69D8\u3005\u306A\u95A2\
  \u6570\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u304A\u308A\u3001\u305D\u308C\u305E\
  \u308C\u7570\u306A\u308B\u76EE\u7684\u306B\u5F79\u7ACB\u3061\u307E\u3059\u3002\u3053\
  \u3053\u3067\u306F\u305D\u306E\u4F7F\u7528\u65B9\u6CD5\u3092\u7D39\u4ECB\u3057\u307E\
  \u3059\uFF1A."
lastmod: '2024-04-05T22:38:41.755237-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A PHP\u306F\u6A19\u6E96\u3067\u3001\u6587\u5B57\u5217\u3092\
  \u5927\u6587\u5B57\u306B\u3059\u308B\u305F\u3081\u306E\u69D8\u3005\u306A\u95A2\u6570\
  \u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u304A\u308A\u3001\u305D\u308C\u305E\u308C\
  \u7570\u306A\u308B\u76EE\u7684\u306B\u5F79\u7ACB\u3061\u307E\u3059\u3002\u3053\u3053\
  \u3067\u306F\u305D\u306E\u4F7F\u7528\u65B9\u6CD5\u3092\u7D39\u4ECB\u3057\u307E\u3059\
  \uFF1A."
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## 方法：
PHPは標準で、文字列を大文字にするための様々な関数をサポートしており、それぞれ異なる目的に役立ちます。ここではその使用方法を紹介します：

### 文字列の最初の文字を大文字にする:
```php
$string = "hello, world!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // 出力: Hello, world!
```

### 各単語の最初の文字を大文字にする:
```php
$string = "hello, world!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // 出力: Hello, World!
```

### 文字列全体を大文字に変換する:
```php
$string = "hello, world!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // 出力: HELLO, WORLD!
```

よりカスタマイズが必要な場合やサードパーティの解決策を利用する場合には、特に基本のASCIIセットを越える文字を扱う国際化の際に、`mbstring`（マルチバイト文字列用）のようなライブラリを利用できます。

### mbstringを使用してUTF-8文字列を大文字にする:
PHP設定で`mbstring`拡張機能が有効になっていることを確認してから：

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // 出力: Élégant
```

このアプローチにより、ASCII以外の文字を含む文字列を、様々な言語のニュアンスに従って正確に大文字にすることができます。
