---
title:    "Elm: コマンドライン引数の読み込み"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読み込むことの重要性は、プログラムの実行時に柔軟性を持たせることができるためです。例えば、異なるオプションを指定することで、プログラムの動作を変えることができます。これにより、同じプログラムでも様々な状況に対応できるようになります。

## 方法

Elmでは、`Command-line-args`モジュールを使用してコマンドライン引数を読み取ることができます。まずは、以下のコードを ```Elm elm - 代わりに ``` で使用できるようにファイルに保存します。

```Elm
import Platform exposing (..)
import CommandlineArgs

main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

-- コマンドライン引数の取得
init : () -> (Model, Cmd Msg)
init _ =
    (Model "default value", Cmd.none)

-- コマンドライン引数の更新
type Msg = UpdateArgs CommandlineArgs.Result

update : Msg -> Model -> (Model, Cmd Msg)
update (UpdateArgs result) _ =
    case result of
        Ok value ->
            (Model value, Cmd.none)

        Err error ->
            -- エラー処理を行う
            (Model "エラーが発生しました", Cmd.none)

-- ビューの表示
type alias Model =
    String

view : Model -> Html Msg
view model =
    text model
```

コマンドライン引数は、次のような形で指定します。

```bash
elm ファイル名 引数1 引数2 ...
```

上記のコードでは、`init`関数でデフォルト値として`"default value"`が設定されています。`update`関数では、指定された引数を取得して`Model`を更新しています。

## 深堀り

`Command-line-args`モジュールでは、様々なオプションを使用することができます。例えば、デフォルト値や必須項目の指定、オプションの詳細な定義などができます。詳細な使い方は[公式ドキュメント](https://package.elm-lang.org/packages/tixxit/command-line-args/latest/)を参照してください。

## 参考リンク

- [公式ドキュメント](https://package.elm-lang.org/packages/tixxit/command-line-args/latest/)
- [GitHubレポジトリ](https://github.com/tixxit/command-line-args)
- [コマンドライン引数の解析方法について](https://qiita.com/bananaumai/items/f87ee226b0fba2c10a9e)
- [Command Line Parsing in Elm](https://medium.com/hackervalleys/command-line-parsing-in-elm-13558a55d34e)

## もっと知りたい場合は？

Elmでは、コマンドライン引数だけでなく様々な機能を備えたモジュールが提供されています。興味がある方は、[Elmパッケージ一覧](https://package.elm-lang.org/)をチェックしてみてください。