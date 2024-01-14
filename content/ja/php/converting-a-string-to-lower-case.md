---
title:    "PHP: 文字列を小文字に変換する"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## なぜ

あなたは、プログラミングの世界ではよく使われるテクニックの1つである文字列を小文字に変換することに魅了されるかもしれません。小文字化は、文字列の整形や比較などの処理に必要不可欠です。

## 方法

文字列を小文字に変換する方法はいくつかありますが、今回はPHPの組み込み関数である`strtolower()`を使ってみましょう。

```
<?php
$input = "HELLO WORLD";
$output = strtolower($input);
echo $output;
```

このコードを実行すると、`hello world`という出力が得られます。入力された文字列が全て小文字に変換されていることが確認できますね。

## 詳細を掘り下げる

`strtolower()`は、引数として与えられた文字列を全て小文字に変換して返す関数です。これは、単純な一文字ごとの変換を行うのではなく、文字列全体を一気に変換するため効率的です。また、英語のアルファベット以外の文字についても、ロケールに応じた変換が行われます。

さらに詳しく知りたい方は、PHP公式ドキュメントの`strtolower()`のページを参照してみてください。

## 参考リンク

- [PHPマニュアル: strtolower()関数](https://www.php.net/manual/ja/function.strtolower.php)
- [PHP Manual: strtolower() function](https://www.php.net/manual/en/function.strtolower.php)
- [How to Use the strtolower() Function in PHP](https://www.w3schools.com/php/func_string_strtolower.asp)