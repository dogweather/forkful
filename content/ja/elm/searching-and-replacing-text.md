---
title:                "テキストの検索と置換"
html_title:           "Elm: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストの検索と置換を行う理由は、繰り返し行わなければいけない作業を自動化することや、大量のテキストを効率的に編集するためです。

## 方法

```elm
-- 起動コマンド
elm repl

-- テキストの置換
String.replace "元のテキスト" "新しいテキスト" "対象のテキスト"

-- マッチした部分のみ置換
String.replace "元のテキスト" "新しいテキスト" "対象のテキスト" True
```

上記のようなコードを実行すると、対象のテキストの中から指定した元のテキストを新しいテキストに置換します。

```elm
> String.replace "cat" "dog" "I have a cat."
"I have a dog."

> String.replace "Hi" "Hello" "Hi everyone!"
"Hello everyone!"

> String.replace "Apple" "Banana" "I love apples, but I also like applesauce."
"I love bananas, but I also like bananasauce."
```

指定した元のテキストが複数回出現する場合でも、全てのマッチした部分が置換されます。また、最後の引数にTrueを渡すと、マッチした部分のみが置換されるようになります。

## ディープダイブ

Elmではテキストの検索と置換にはStringモジュールを使用します。Stringモジュールの中には、文字列を操作するための便利な関数がたくさん用意されています。また、Stringモジュールに限らず、他のモジュールでもテキストの置換に役立つ関数が提供されています。詳しくは公式ドキュメントを参照してください。

## 関連リンク

- [Elm公式ドキュメント](https://guide.elm-lang.org/)
- [Stringモジュールのドキュメント](https://package.elm-lang.org/packages/elm/core/latest/String)
- [文字列操作に便利なElmライブラリ](https://github.com/mgold/elm-string-extra)