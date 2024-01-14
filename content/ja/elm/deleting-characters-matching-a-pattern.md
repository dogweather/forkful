---
title:    "Elm: パターンに一致する文字を削除する"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# なぜ
なぜ誰かが特定のパターンに一致する文字を削除するのかについて説明します。この作業を行うことで、より効率的で読みやすいコードを作成することができます。

## 方法
まず、私たちは特定のパターンに一致する文字を削除するために、Elmの組み込み関数である`String.filter`を使用します。ここでは、削除したい文字を含まない新しい文字列を作成するように指示することができます。

```Elm
let
  newString =
    String.filter (\character -> character /= "a") "abcdefg"
in
  newString
```

このコードを実行すると、以下のような出力が得られます。

```Elm
"bcdefg"
```

さらに、正規表現を使用することで、特定のパターンに一致する文字を削除することもできます。

```Elm
-- "a"または"b"または"c"に一致する文字を削除する
let
  newString =
    Regex.replace Regex.All (Regex.regex "[abc]") "abcdefg" (always "")
in
  newString
```

このコードを実行すると、以下のような出力が得られます。

```Elm
"defg"
```

## 深堀り
文字の削除についてさらに詳しく掘り下げると、削除操作は文字列の不変性を保つことに注意する必要があります。つまり、元の文字列を変更するのではなく、新しい文字列を作成する必要があります。

また、パターンにマッチする文字の位置を特定することで、より正確な削除が可能となります。そのためには、`String.indexes`関数を使用することで、マッチした文字のインデックスをリストで取得することができます。

```Elm
-- "a"または"b"に一致する文字のインデックスを表示する
String.indexes [String.Index.fromInt 0] (Regex.regex "[ab]") "abcdefg"
```

このコードを実行すると、以下のようにインデックスのリストが返されます。

```Elm
[ String.Index.fromInt 0, String.Index.fromInt 1 ]
```

## 関連情報を参照
- [Elm公式ドキュメ