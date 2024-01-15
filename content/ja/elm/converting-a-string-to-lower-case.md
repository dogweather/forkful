---
title:                "文字列を小文字に変換する"
html_title:           "Elm: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

文字列を小文字に変換する理由は様々です。例えば、ユーザーからの入力を一貫性のあるフォーマットに変換する必要がある場合や、ソートや比較のために文字列を正規化する必要がある場合には便利です。

## How To

まず、`String.toLower`関数を使用して文字列を小文字に変換します。

```Elm
import String

String.toLower "HELLO WORLD" 
```

出力は`"hello world"`になります。

また、リスト内の全ての文字列を小文字に変換することもできます。

```Elm
import String

List.map String.toLower ["CAT", "DOG", "FISH"]
```

出力は`["cat", "dog", "fish"]`になります。

## Deep Dive

`String.toLower`関数は、内部でUnicodeの`String.foldl`関数を使用しています。これにより、どんな言語でも正確に小文字に変換されます。ただし、アクセントや記号などは除外されるので注意が必要です。

また、`String.toLower`関数はイミュータブルなので、元の文字列は変更されません。新しい文字列のコピーが作成されるため、パフォーマンスには影響します。

## See Also

- [Elm公式ドキュメント - String](https://elm-lang.org/docs/strings)
- [Unicodeの小文字変換アルゴリズムについて](https://unicode.org/faq/casemap_charprop.html#28)