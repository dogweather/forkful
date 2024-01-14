---
title:    "Haskell: 文字列の連結"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
あなたはたぶん、プログラミングをするときに、文字列を結合する必要があります。例えば、ユーザーの入力を受け取り、それをメッセージとして表示する場合などです。文字列を結合することで、より柔軟にメッセージを作ることができます。

## How To
文字列を結合するには、Haskellの```++```演算子を使用します。以下のコードは、```"Hello"```と```"World"```を結合して表示する例です。

```Haskell
main = do
  let greeting = "Hello" ++ "World"
  putStrLn greeting
```
結果：HelloWorld

さらに、複数の文字列を結合することもできます。例えば、```"I am"```と```"21"```という文字列の間に空白を入れて結合する場合は次のようになります。

```Haskell
main = do
  let age = "21"
  let sentence = "I am " ++ age ++ " years old"
  putStrLn sentence
```
結果：I am 21 years old

## Deep Dive
Haskellの```++```演算子は、リストの連結にも使用できます。これは、文字列が順番にリストとして扱われるためです。例えば、```"Hello"```という文字列は```['H', 'e', 'l', 'l', 'o']```というリストとして扱われます。

この性質を利用して、```++```演算子を使って複数の文字列を結合することができます。例えば、以下のように複数の文字列をリストとして定義し、それらを結合することができます。

```Haskell
main = do
  let name = "John"
  let middleName = "Doe"
  let lastName = "Smith"
  let fullName = name ++ " " ++ middleName ++ " " ++ lastName
  putStrLn fullName
```
結果：John Doe Smith

## See Also
- Haskellの基本的な演算子の使い方：[https://www.flambda.net/posts/2015-04-01-haskell.html](https://www.flambda.net/posts/2015-04-01-haskell.html)
- 文字列操作に関するHaskellの詳細な情報：[https://www.haskell.org/tutorial/strings.html](https://www.haskell.org/tutorial/strings.html)
- 文字列結合以外のHaskellのリスト操作：[https://wiki.haskell.org/Keywords#Lists](https://wiki.haskell.org/Keywords#Lists)