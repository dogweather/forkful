---
title:                "文字列を連結する"
html_title:           "Elixir: 文字列を連結する"
simple_title:         "文字列を連結する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

# 何 & なぜ？
文字列を連結することは、複数の文字列を一つの文字列に結合することを意味します。プログラマーは、より複雑な文字列を作成するために、この方法を使用します。

## 方法：
```Elixir 
"Hello " <> "world!"
```
```Elixir
  "Hello " <> "world!" <> " How are you?"
```
出力：
```
Hello world!
Hello world! How are you?
```

## 深堀り：
1. "Hello " <> "world!" のように、Elixirでは"<> "演算子を使用して文字列を連結することができます。
2. 文字列が多い場合は、ElixirのStringモジュールのjoin関数を使用して、効率的に文字列を連結することができます。
3. 文字列連結の代替手段として、ElixirではIOモジュールのprint関数を使用することもできます。

## 関連情報：
- [Elixir公式ドキュメント](https://elixir-lang.org/getting-started/basic-types.html#strings)
- [Elixirスタイルガイド](https://github.com/christopheradams/elixir_style_guide#concatenation)

## 以上：
以上は、Elixirの文字列連結の方法についての簡単な説明でした。より詳細な情報が必要な場合は、関連情報を参照することをお勧めします。