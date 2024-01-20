---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？ ("What & Why?")

文字列内挿入は、文字列の中で変数値を直接使用するプログラミング技術です。プログラマーは、コードの可読性を高め、エラーを減らすためにこれを使用します。

## どうやって？ ("How to:")

これは、どのようにしてHaskellで文字列の内挿を行うかを示しています。次の例を見てみましょう。

```haskell
import Text.Printf (printf)

main :: IO ()
main = do
  let name = "John"
  let age = 20
  print $ printf "My name is %s and I am %d years old." name age
```
このコードの出力は次のようになります。

```
"My name is John and I am 20 years old."
```

## 詳細情報 ("Deep Dive")

文字列内挿入は、古代のプログラミング言語から存在し、コードの可読性を改善するために広く使われてきました。

Haskellでは、`Text.Printf`モジュールの`printf`関数を使用して文字列内挿入を行います。しかし、アルタナティブとして`Data.Text`の`intercalate`や`concat`関数などが存在します。

上記の例では、`printf`関数を使用していますが、テンプレートの記述が型セーフでないため、ランタイムエラーを引き起こす可能性があります。

## 関連情報 ("See Also")

1. `printf`関数の詳細: [Haskellでprintfを使う](https://www.haskell.org/tutorial/printf.html)
2. `Text.Printf`モジュールの詳細: [HaskellのText.Printfモジュール](https://hackage.haskell.org/package/base-4.14.1.0/docs/Text-Printf.html)
3. Haskellでの文字列連結: [Haskellで文字列をつなげる](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/string-concatenation)