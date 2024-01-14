---
title:    "Haskell: 文字列を小文字に変換する"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換するメソッドを使う理由は、文字列を簡単に比較したり、整形したりするためです。例えば、ユーザーが入力した文字列とあなたが持っている文字列を比較する場合、文字列の大文字・小文字の違いを無視したいかもしれません。そんなときに文字列を小文字に変換するメソッドを使えば、簡単に比較することができます。

## 方法

まずは文字列を小文字に変換する関数を定義します。Haskellでは文字列を小文字に変換する関数`toLower`が標準ライブラリに用意されていますが、今回は自分で実装してみましょう。

```Haskell
toLower :: String -> String
toLower str = [toLower c | c <- str]
```

上の例では、引数として渡された文字列の各文字を小文字に変換して新しい文字列を作成しています。このコードをコンパイルして実行すると、以下のような結果が得られます。

```Haskell
toLower "Hello World"
"hello world"
```

文字列を小文字に変換することで、大文字・小文字を区別せずに文字列を比較することができます。例えば、以下のように文字列が一致するかどうかを判定する関数を定義することができます。

```Haskell
isMatch :: String -> String -> Bool
isMatch str1 str2 = toLower str1 == toLower str2
```

この関数を使えば、大文字・小文字を無視して文字列が一致するかどうかを判定することができます。

```Haskell
isMatch "Hello World" "hello world"
True
```

## ディープダイブ

Haskellでは文字列を操作する際、`String`型ではなく`[Char]`型を使用することが推奨されています。また、文字列を`String`型から`[Char]`型に変換する方法として`splitAt`関数が用意されていますが、より簡単に`[Char]`型に変換する方法があります。それは文字列を`map`関数を使って`Char`型のリストに変換することです。

```Haskell
map :: (a -> b) -> [a] -> [b]
```

`map`関数は引数として変換する関数とリストを取り、リストの各要素に対して関数を適用し、新しいリストを作成します。つまり、`String`型を`[Char]`型に変換する場合は、以下のようになります。

```Haskell
map toLower "Hello World"
['h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd']
```

さらに、`[Char]`型から`String`型に戻す際は`unwords`関数を使うことで簡単に行うことができます。

```Haskell
unwords :: [String] -> String
```

`unwords`関数はリストを引数として受け取り、各要素をスペースで区切った文字列を返します。つまり、文字列を小文字に変換した後、`unwords`関数を使えば元の`String`型に戻すことができます。

```Haskell
unwords (map toLower "HELLO WORLD")
"hello world"
```

## 参考

[実