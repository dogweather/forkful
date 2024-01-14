---
title:    "Haskell: 「文字列を小文字に変換する」"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# なぜ

文字列を小文字に変換することによって、コード内の文字列の比較や検索がより簡単になります。

## 方法

```haskell
-- 文字列を小文字に変換する関数
toLower :: String -> String
toLower [] = []
toLower (x:xs) = toLowerChar x : toLower xs

-- 文字を小文字に変換する関数
toLowerChar :: Char -> Char
toLowerChar c 
    | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32) -- 英大文字の場合、小文字に変換
    | otherwise = c -- その他の文字はそのまま返す
```

入力と出力の例を以下に示します。

```haskell
toLower "Hello World!" -- => "hello world!"
toLower "Today is a Sunny day." -- => "today is a sunny day."
```

## 深堀り

Haskellでは、文字列を小文字に変換する関数が標準ライブラリに用意されていません。そのため、自分で関数を定義する必要があります。例で示した関数は、再帰を使って文字列の各文字を小文字に変換しています。また、このような場合にはパターンマッチングを使うことでスマートなコードを書くことができます。

しかし、英語以外の言語では大文字と小文字が区別されない場合もあるため、文字列を小文字に変換することが必要ない場合もあります。そのような場合には、Haskellの標準ライブラリに用意されている `map` 関数を使うことで簡単に小文字に変換することができます。

## 関連リンク

* [Haskell Documentation: String Operations](https://www.haskell.org/onlinereport/standard-prelude.html#t:string)
* [Real World Haskell: More Types, Type Classes](http://book.realworldhaskell.org/read/more-types-type-classes.html)