---
date: 2024-01-20 17:51:29.476318-07:00
description: "\u6587\u5B57\u5217\u5C55\u958B\u3068\u306F\u3001\u6587\u5B57\u5217\u306E\
  \u4E2D\u306B\u5909\u6570\u306E\u5185\u5BB9\u3084\u8A08\u7B97\u7D50\u679C\u3092\u633F\
  \u5165\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\
  \u3001\u52D5\u7684\u306A\u30C6\u30AD\u30B9\u30C8\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\
  \u751F\u6210\u3057\u305F\u308A\u3001\u51FA\u529B\u3092\u3088\u308A\u8AAD\u307F\u3084\
  \u3059\u304F\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.796999-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u5C55\u958B\u3068\u306F\u3001\u6587\u5B57\u5217\u306E\
  \u4E2D\u306B\u5909\u6570\u306E\u5185\u5BB9\u3084\u8A08\u7B97\u7D50\u679C\u3092\u633F\
  \u5165\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\
  \u3001\u52D5\u7684\u306A\u30C6\u30AD\u30B9\u30C8\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\
  \u751F\u6210\u3057\u305F\u308A\u3001\u51FA\u529B\u3092\u3088\u308A\u8AAD\u307F\u3084\
  \u3059\u304F\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)

文字列展開とは、文字列の中に変数の内容や計算結果を挿入することです。プログラマは、動的なテキストメッセージを生成したり、出力をより読みやすくするためにこれを行います。

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
