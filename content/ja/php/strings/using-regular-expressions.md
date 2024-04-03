---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:55.001320-07:00
description: "\u4F7F\u3044\u65B9: PHP\u306FPCRE\uFF08Perl\u3068\u4E92\u63DB\u6027\u306E\
  \u3042\u308B\u6B63\u898F\u8868\u73FE\uFF09\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u901A\
  \u3058\u3066\u6B63\u898F\u8868\u73FE\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u304A\
  \u308A\u3001\u8C4A\u5BCC\u306A\u95A2\u6570\u30BB\u30C3\u30C8\u3092\u63D0\u4F9B\u3057\
  \u3066\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u304C\u305D\u306E\u4F7F\u7528\u65B9\u6CD5\
  \u3067\u3059\uFF1A #."
lastmod: '2024-03-13T22:44:42.231395-06:00'
model: gpt-4-0125-preview
summary: "PHP\u306FPCRE\uFF08Perl\u3068\u4E92\u63DB\u6027\u306E\u3042\u308B\u6B63\u898F\
  \u8868\u73FE\uFF09\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u901A\u3058\u3066\u6B63\u898F\
  \u8868\u73FE\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u304A\u308A\u3001\u8C4A\u5BCC\
  \u306A\u95A2\u6570\u30BB\u30C3\u30C8\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\
  \u3002\u4EE5\u4E0B\u304C\u305D\u306E\u4F7F\u7528\u65B9\u6CD5\u3067\u3059\uFF1A\n\
  \n#."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 使い方:
PHPはPCRE（Perlと互換性のある正規表現）ライブラリを通じて正規表現をサポートしており、豊富な関数セットを提供しています。以下がその使用方法です：

### パターンのマッチング:
文字列内にパターンが存在するかどうかを確認するには、`preg_match()`を使用します。この関数は、文字列内にパターンが見つかった場合は1を、見つからなかった場合は0を返します。

```php
if (preg_match("/\bweb\b/i", "PHPはウェブスクリプト言語です")) {
    echo "マッチが見つかりました。";
} else {
    echo "マッチが見つかりませんでした。";
}
// 出力: マッチが見つかりました。
```

### すべてのマッチを見つける:
`preg_match_all()`は、文字列内のパターンのすべての出現を見つける必要がある場合に使用されます。

```php
$text = "猫と犬";
$pattern = "/\b([a-z]+)\b/i";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// 出力: Array ( [0] => cats [1] => and [2] => dogs )
```

### テキストの置換:
正規表現にマッチするテキストを置き換えるには、`preg_replace()`が使用されます。これはデータのフォーマットやクリーニングに非常に強力です。

```php
$originalText = "2003年4月15日";
$pattern = "/(\w+) (\d+), (\d+)/i";
$replacement = '${1}1,$3';
echo preg_replace($pattern, $replacement, $originalText);
// 出力: 2003年4月1,15
```

### 文字列の分割:
`preg_split()`を使用して、デリミタとしてパターンを指定して文字列を配列に分割できます。

```php
$text = "PHPは、非常に人気のある、スクリプト言語です";
$parts = preg_split("/,\s*/", $text);
print_r($parts);
// 出力: Array ( [0] => PHPは [1] => 非常に人気のある [2] => スクリプト言語です )
```

さらに、複雑なregexパターンやタスクについては、Symfonyの`Finder`コンポーネントやLaravelのヘルパー関数のコレクションなどのフレームワークやライブラリがより便利な抽象化層を提供するかもしれません。しかし、PHPスクリプト内での効率的なテキスト処理や検証のためには、PHPの組み込みPCRE関数を理解して利用することが重要です。
