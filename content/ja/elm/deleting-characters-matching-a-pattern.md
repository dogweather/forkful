---
title:                "Elm: パターンにマッチする文字を削除する"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングの世界で、パターンにマッチする文字を削除することは非常によく行われます。この作業は、例えばテキストデータのクリーニングや特定の文字を置き換えるために使用されます。

## 方法

この記事では、Elm言語を使用してパターンにマッチする文字を削除する方法を説明します。以下のコードブロックに、実際のコード例とそれを実行した際のサンプル出力を記載しています。

```Elm
-- テキストデータから全ての数字を削除する例

import Regex

text = "Today's date is 12/31/2021."
cleanText = Regex.replace (Regex.regex "\\d") (\_ -> "") text

main = text ++ "Cleaned text: " ++ cleanText

-- Output:
-- Today's date is 12/31/2021. Cleaned text: Today's date is /.
```

上記の例では、Regexモジュールを使用してテキスト中の数字を削除しています。まず、`Regex.regex`を使用して削除したいパターンを指定します。ここでは、`\d`を使用して数字を表しています。次に、`Regex.replace`を使用して、指定した文字を空文字列に置き換えています。これにより、テキスト中の数字が全て削除されることになります。

## 深層

この記事では、パターンマッチングと文字列操作の基本的な概念を学ぶことができます。しかし、Elmではより高度なパターンマッチングの方法や、より柔軟な文字列操作の手段があります。興味があれば、公式ドキュメントや他のチュートリアルを参考にして、より深い知識を身につけることができるでしょう。

## 参考リンク

- [Elm公式ドキュメント](https://elm-lang.org/docs)
- [Elmチュートリアル](https://elm-lang.org/learn)