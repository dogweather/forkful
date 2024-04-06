---
date: 2024-01-20 17:51:29.476318-07:00
description: "How to: (\u3084\u308A\u65B9) \u6587\u5B57\u5217\u5C55\u958B\u306F PHP\
  \ 4 \u304B\u3089\u5229\u7528\u53EF\u80FD\u3067\u3001\u7C21\u5358\u306B\u5909\u6570\
  \u5024\u3092\u6587\u5B57\u5217\u5185\u306B\u7D44\u307F\u8FBC\u3081\u308B\u6A5F\u80FD\
  \u3067\u3059\u3002\u4F8B\u3048\u3070\u3001\u30CF\u30FC\u30C9\u30B3\u30FC\u30C7\u30A3\
  \u30F3\u30B0\u3055\u308C\u305F\u6587\u5B57\u5217\u306E\u4EE3\u308F\u308A\u306B\u3001\
  \u30E6\u30FC\u30B6\u30FC\u540D\u3084\u8A2D\u5B9A\u306A\u3069\u3001\u5909\u308F\u308B\
  \u53EF\u80FD\u6027\u304C\u3042\u308B\u30C7\u30FC\u30BF\u3092\u4F7F\u7528\u3059\u308B\
  \u5834\u5408\u306B\u4FBF\u5229\u3067\u3059\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:41.759138-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u6587\u5B57\u5217\u5C55\u958B\u306F PHP 4 \u304B\u3089\
  \u5229\u7528\u53EF\u80FD\u3067\u3001\u7C21\u5358\u306B\u5909\u6570\u5024\u3092\u6587\
  \u5B57\u5217\u5185\u306B\u7D44\u307F\u8FBC\u3081\u308B\u6A5F\u80FD\u3067\u3059\u3002\
  \u4F8B\u3048\u3070\u3001\u30CF\u30FC\u30C9\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\u3055\
  \u308C\u305F\u6587\u5B57\u5217\u306E\u4EE3\u308F\u308A\u306B\u3001\u30E6\u30FC\u30B6\
  \u30FC\u540D\u3084\u8A2D\u5B9A\u306A\u3069\u3001\u5909\u308F\u308B\u53EF\u80FD\u6027\
  \u304C\u3042\u308B\u30C7\u30FC\u30BF\u3092\u4F7F\u7528\u3059\u308B\u5834\u5408\u306B\
  \u4FBF\u5229\u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

## How to: (やり方)
```PHP
<?php
$name = "Taro";
$age = 20;

// ダブルクォートを使用する
echo "こんにちは、${name}さん。あなたの年齢は${age}歳です。\n";

// シンプルな構文
echo "ハロー、$name。来年は " . ($age + 1) . " 歳になるね。\n";
?>

// 出力:
// こんにちは、Taroさん。あなたの年齢は20歳です。
// ハロー、Taro。来年は 21 歳になるね。
```

## Deep Dive (深掘り)
文字列展開は PHP 4 から利用可能で、簡単に変数値を文字列内に組み込める機能です。例えば、ハードコーディングされた文字列の代わりに、ユーザー名や設定など、変わる可能性があるデータを使用する場合に便利です。

代替手段として、コンカテネーション（点連結）を使えますが、コードが読みにくくなる可能性があります。また、`printf()` や `sprintf()` 関数を使用して、より複雑なフォーマットを生成することもできます。

文字列展開を行う際、ダブルクォートやヒアドキュメント内でのみ動作し、シングルクォートでは動作しません。変数が複雑な場合（例: 配列の要素やオブジェクトのプロパティ）は、波括弧 `{}` を使用して明示しなければならないことがあります。

## See Also (関連情報)
- PHP公式ドキュメントの文字列セクション: [PHP: Strings - Manual](https://www.php.net/manual/en/language.types.string.php)
- PHPの `printf` 関数について学ぶ: [PHP: printf - Manual](https://www.php.net/manual/en/function.printf.php)
- 変数のパースと文字列の展開: [PHP: Variable parsing - Manual](https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing)
