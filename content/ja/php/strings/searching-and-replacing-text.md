---
date: 2024-01-20 17:58:17.992038-07:00
description: "How to: \u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\
  \u306F\u53E4\u304F\u304B\u3089\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u5F79\
  \u7ACB\u3063\u3066\u3044\u307E\u3059\u3002PHP\u3067\u306F`str_replace`\u95A2\u6570\
  \u304C\u3053\u306E\u76EE\u7684\u3067\u3088\u304F\u4F7F\u308F\u308C\u307E\u3059\u3002\
  \u6B63\u898F\u8868\u73FE\u304C\u5FC5\u8981\u306A\u8907\u96D1\u306A\u30D1\u30BF\u30FC\
  \u30F3\u306B\u306F`preg_replace`\u95A2\u6570\u304C\u5229\u7528\u3055\u308C\u307E\
  \u3059\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.143922-06:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u53E4\
  \u304F\u304B\u3089\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u5F79\u7ACB\u3063\
  \u3066\u3044\u307E\u3059\u3002PHP\u3067\u306F`str_replace`\u95A2\u6570\u304C\u3053\
  \u306E\u76EE\u7684\u3067\u3088\u304F\u4F7F\u308F\u308C\u307E\u3059\u3002\u6B63\u898F\
  \u8868\u73FE\u304C\u5FC5\u8981\u306A\u8907\u96D1\u306A\u30D1\u30BF\u30FC\u30F3\u306B\
  \u306F`preg_replace`\u95A2\u6570\u304C\u5229\u7528\u3055\u308C\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

## How to:
以下のコード例を参照してください:

```php
<?php
// 元のテキスト
$text = 'こんにちは、世界！';

// str_replaceを使用してテキストを置換
$replacedText = str_replace('世界', 'プログラミングの世界', $text);

echo $replacedText; // 出力: こんにちは、プログラミングの世界！
?>
```

## Deep Dive
テキストの検索と置換は古くからプログラミングに役立っています。PHPでは`str_replace`関数がこの目的でよく使われます。正規表現が必要な複雑なパターンには`preg_replace`関数が利用されます。 `str_replace`は単純な文字列の置換に使うべきで、大規模なテキストデータには`strtr`関数が効率的かもしれません。

## See Also
- PHP Manual on `str_replace`: https://www.php.net/manual/en/function.str-replace.php
- PHP Manual on `preg_replace`: https://www.php.net/manual/en/function.preg-replace.php
- PHP Manual on `strtr`: https://www.php.net/manual/en/function.strtr.php
