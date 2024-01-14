---
title:                "PHP: 文字列の大文字化"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列を大文字に変換することをするのか、その理由を説明します。

## 方法
文字列の大文字変換についてのコーディング例と、```PHP ... ```のコードブロック内でのサンプル出力を示します。

```
// オリジナルの文字列を定義
$str = "hello world";

// 文字列を大文字に変換する
$new_str = strtoupper($str);

// 変換後の文字列を出力
echo $new_str;

// 出力結果
HELLO WORLD
```

## ディープダイブ
文字列を大文字変換する機能について、より詳細な情報をご紹介します。

テキストエディタやオンラインツールでは、一度に複数の文字列を大文字変換することができます。また、大文字変換の際にはASCII文字のみを対象とするか、異なるロケールの文字にも対応するかを指定することも可能です。

## 参考リンク
- [PHP公式ドキュメント: strtoupper関数](https://www.php.net/manual/ja/function.strtoupper.php)
- [W3Schools: PHP strtoupper() Function](https://www.w3schools.com/php/func_string_strtoupper.asp)
- [TechAcademy: PHPの文字列を大文字、小文字変更する関数基礎まとめ](https://techacademy.jp/magazine/13604)
 
## 参考
参考になると思われる他の記事やリンクを紹介します。