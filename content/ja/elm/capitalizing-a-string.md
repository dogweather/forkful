---
title:    "Elm: 文字列を大文字にする"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングを行う上で、文字列を大文字に変換する必要がある場面があります。そのような状況では、効率的に文字列を大文字に変換する方法を知ることが重要です。

## 方法

コンピューターの世界では、文字列を大文字に変換するのは非常に簡単です。Elmでは、文字列を大文字にするには `String.toUpper` メソッドを使用します。例えば、以下のようにコードを記述することで、文字列を大文字に変換することができます。

```elm
String.toUpper "hello" -- Output: "HELLO"
```

また、必要に応じて、文字列を小文字に変換する方法もあります。`String.toLower` メソッドを使用することで、以下のようにコードを記述することで、文字列を小文字に変換することができます。

```elm
String.toLower "HELLO" -- Output: "hello"
```

## 深く掘り下げる

Elmでは、文字列を大文字や小文字に変換するメソッドの他にも、様々な文字列操作に役立つメソッドが存在します。例えば、文字列の長さを取得する `String.length` や、指定した文字列が含まれるかどうかを判定する `String.contains` メソッドが代表的です。これらのメソッドも活用することで、より柔軟に文字列を操作できるようになります。

## 参考リンク

- [Elm公式ドキュメント](https://guide.elm-lang.org)
- [Elmで文字列操作をする方法](https://qiita.com/shinnoki/items/5d260b1d9961c8c1c7c2)
- [Elmで文字列を処理する方法](https://qiita.com/matsubo/items/5677c4605bf1338d9cc7)

## 関連リンク

- [Elmを使ってWebアプリを作ろう！](https://www.udemy.com/elm-webapp/) (Udemy)
- [Elmを学ぶためのガイド](https://dev.to/lucamug/learn-elm-from-scratch-2019-4eh5) (Dev.to)
- [Elm入門コース](https://egghead.io/browse/frameworks/elm) (Egghead)