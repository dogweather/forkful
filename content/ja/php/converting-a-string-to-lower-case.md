---
title:    "PHP: 「文字列を小文字に変換する」"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換するのに関わる理由について、わずか1〜2文で説明します。

## 変換の方法
「```PHP ...```」のコードブロック内でコーディング例やサンプルの出力を示します。

例：
```
$name = "JOHN SMITH";
echo strtolower($name);
//出力： john smith
```

## 深く掘り下げる
文字列を小文字に変換する際に注意すべきことや便利な関数など、より詳しい情報を提供します。

例：
- 文字列の特定の文字を大文字から小文字に変換するには、`lcfirst()`関数を使用します。
- 日本語や多言語の文字列を小文字に変換する際には、`mb_strtolower()`関数を使うと正しい結果が得られます。

## 参考
[PHPのstrtolower()関数ドキュメント](https://www.php.net/manual/ja/function.strtolower.php)

[PHPのmb_strtolower()関数ドキュメント](https://www.php.net/manual/ja/function.mb-strtolower.php)

## 参照
- [PHPで文字列を小文字に変換する方法](https://www.sejuku.net/blog/74002)
- [文字列から軽い前処理をして判定する方法](https://qiita.com/NAVER_corporation/items/9472a1cfffcf279d3e01)
- [Rubyで文字列を小文字に変換する方法](https://www.rubyguides.com/2015/05/ruby-string-methods/)
- [Pythonで文字列を小文字に変換する方法](https://docs.python.org/ja/3/library/stdtypes.html#string-methods)