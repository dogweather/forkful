---
title:                "Elm: 「文字列の連結」"
simple_title:         "「文字列の連結」"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

# なぜ

なぜ文字列を連結するのか、説明します。文字列の連結は非常に一般的なタスクであり、プログラム内の情報を組み合わせる際によく使われます。例えば、ユーザーの名前とアドレスを結合して、メッセージを表示することができます。

# 連結の仕方

```Elm
-- 文字列を連結する
concatenateStrings str1 str2 =
  str1 ++ str2

-- サンプルコード
main =
  let
    name = "田中"
    address = "東京都渋谷区"
  in
    "こんにちは、" ++ name ++ "さん！あなたの住所は" ++ address ++ "ですね。"
```

このコードでは、`concatenateStrings`という関数を定義しています。この関数は、引数として与えられた2つの文字列を連結して返します。そして、`main`関数内では、`name`と`address`という2つの変数を定義し、`concatenateStrings`を使って文字列を連結することで、メッセージを表示しています。

このコードを実行すると、以下のような出力が得られます。

```
こんにちは、田中さん！あなたの住所は東京都渋谷区ですね。
```

# 深堀り

Elmでは、文字列を連結する方法として、`++`という演算子が用意されています。`++`は左側の文字列に右側の文字列を連結することができます。また、複数の文字列を連結する場合は、`++`を連続して使うこともできます。

また、文字列を連結する際には、`concat`という組み込み関数も使うことができます。例えば、以下のようになります。

```Elm
concat ["こんにちは", "田中", "さん！"] -- こんにちは田中さん！
```

文字列を結合する際には、特に気を付ける必要はありませんが、「＋」を使って文字列を結合することはできません。`＋`は数値を足すための演算子であり、文字列を結合することはできません。

# おすすめリンク

- [Elm公式ドキュメント](https://guide.elm-lang.org/core_language.html#concatenation)
- [Elm入門ガイド (日本語)](https://elm-jp.org/introduction.html)
- [Elm入門チュートリアル (日本語)](https://www.yoheim.net/blog.php?q=20190101)
- [Elmビギナートレーニング (日本語)](https://boennemann.github.io/elm-training-jp/)