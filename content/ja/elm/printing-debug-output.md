---
title:                "Elm: デバッグ出力のプリント"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

デバッグ用の出力を出すことは、プログラムの開発やデバッグをする上で非常に重要なことです。そのため、Elmプログラミングにおいてもデバッグ用の出力は欠かせません。デバッグ用の出力をすることで、プログラムの動きをより詳細に把握し、バグを見つけやすくなります。

## 使い方

デバッグ用の出力を行うには、 ```Debug.log``` 関数を使用します。この関数は、1つの引数として任意の値を受け取り、その値をコンソールに出力します。使用例を示します。

```Elm
-- コンソールに "Hello World!" という文字列を出力する
Debug.log "デバッグ用出力" "Hello World!"
```

このように、コンソールに出力されるメッセージの前には、デバッグ用のタグを付けることができます。これにより、出力されたメッセージを特定することが容易になります。

## 詳細を掘り下げる

デバッグ用の出力を行う際、より詳細な情報を確認したい場合があります。そのような場合には、 ```Debug.inspect``` 関数を使用することができます。この関数は、任意の値を受け取り、その値をコンソールに出力すると同時に、その値をデバッグ用のタグ付きの文字列として返します。使用例を示します。

```Elm
-- コンソールに "変数xの値は: 10" というメッセージを出力し、変数xの値を取得する
let
    x = 10
in
    ( x
        |> Debug.inspect "変数xの値は: "
    )
```

このようにすることで、変数の値を出力するだけでなく、その値を別の部分で使用することも可能になります。

## その他のリソース

- [Elm Documentation: Debugging](https://guide.elm-lang.org/debugging/)
- [Debugging with Elm](https://thoughtbot.com/blog/debugging-with-elm)
- [Debugging Elm Code](https://www.snoyman.com/blog/2017/08/debugging-elm-code)

## 関連リンク

- [Elm 公式ドキュメント](https://guide.elm-lang.org/)
- [Elm 公式フォーラム](https://discourse.elm-lang.org/)
- [Elm Japan コミュニティー](https://elmjapan.org/)