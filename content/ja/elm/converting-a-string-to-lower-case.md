---
title:    "Elm: 「stringを小文字に変換する」"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換することに関わる理由は、多岐に渡りますが、例えばフォームの入力を正規化したり、文字列の比較を行う際に一貫性を持たせるためなどが挙げられます。

## 方法

まず、Elmコードを書く前に、文字列を小文字に変換する必要性があるかを考える必要があります。入力フォームにおいて、ユーザーが大文字と小文字を区別しないで入力した場合、文字列を小文字化することで、一貫性のあるデータを保持することができます。

```elm
import String

text = "Elm Programming"

lowercasedText = String.toLower text
```

上記のコードを実行すると、以下のような出力が得られます。

```elm
"elm programming"
```

また、もし文字列内にエスケープシーケンスが含まれている場合は、`Grapheme.toLower`を使用することで、エスケープシーケンスを無視して小文字化することができます。

```elm
import String
import Grapheme

text = "Élm Programming"

lowercasedText = Grapheme.toLower text
```

実行結果は以下のようになります。

```elm
"élm programming"
```

## 深堀

Elmで文字列を小文字に変換する方法は、2つの主な方法があります。1つ目は、`String.toLower`を使用する方法で、2つ目は、`Grapheme.toLower`を使用する方法です。

`String.toLower`は、文字列を単純に小文字に変換することができますが、エスケープシーケンスを無視するため、意図しない結果を得ることがあります。一方、`Grapheme.toLower`はエスケープシーケンスを無視せず、より正確に小文字化することができます。

文字列操作を行う際には、入力データに対して慎重に考え、適切なメソッドを選択することが重要です。

## 関連情報

- [Elm公式ドキュメント - 文字列操作](https://guide.elm-lang.jp/appendix/string.html)
- [Elm Japan User Group](https://elmjapan.org/)
- [HackSmiths](https://www.hacksmiths.io/elm/convert-string-to-lowercase)

## 参考文献

- "A straightforward Elm package to transform our string into lowercase" by Eugene Obrezkov, Medium, 23 April, 2020, https://medium.com/@ghaiklor/elm-how-to-string-to-lowercase-f45ec5a39f7f