---
title:                "コマンドライン引数の読み込み"
html_title:           "Elm: コマンドライン引数の読み込み"
simple_title:         "コマンドライン引数の読み込み"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読み取ることは、Elmプログラミングにおいて非常に便利な能力です。簡単なコマンドラインツールを作成したり、パラメーターを渡すことであなたのプログラムをカスタマイズすることができます。

## 使い方

コマンドライン引数を読み取るには、まずElmの`Platform.worker`を使って、コマンドライン引数を環境変数として公開する任意のアウトプットスタイルを実装する必要があります。

さらに、アプリケーションの`main`関数を次のように書くことで、コマンドライン引数を読み取ることができます。

```Elm
main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , react =
            Platform.platformCmdToCmd
                >> Cmd.map Cmd.map
                >> Maybe.fromJust
        }
```

そして、コマンドライン引数を取得するための関数を作ります：

```Elm
getArguments : (List String -> msg) -> Sub msg
getArguments msg =
    Sub.batch
        [ Sub.map msg (Platform.worker (\_ -> [])
        , Sub.map msg
            (Platform.worker
                (\{ actual } -> Maybe.withDefault [] actual)
                >> Maybe.map (List.map String.fromCharList)
            )
                |> Maybe.withDefault []
        ]
        |> Sub.batch

subscriptions : Model -> Sub Msg
subscriptions model =
    getArguments SetArguments
```

これで、コマンドライン引数をモデルに保存することができます。例えば、引数を使って特定のファイルを開くようなアプリケーションを作成することができます。

## 深く掘り下げる

コマンドライン引数を読み取る組み込みの方法以外にも、外部のJavaScriptライブラリを使うことでさまざまなカスタマイズが可能です。例えば、`elm-cli-options-parser`を使うことでより高度な引数のパースやバリデーションを行うことができます。また、npmパッケージを介して`elm-flags`を使うことで、アプリケーションに静的なコマンドライン引数を含めることもできます。

## 関連情報を見る

- [Elmのドキュメンテーション](https://guide.elm-lang.org/)
- [Elmの公式サイト](https://elm-lang.org/)
- [elm-cli-options-parser](https://package.elm-lang.org/packages/jxxcarlson/elm-cli-options-parser/latest/)
- [elm-flags](https://github.com/uriva/elm-flags)