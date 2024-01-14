---
title:    "Elm: ディレクトリが存在するかどうかをチェックする"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## なぜ
ディレクトリが存在するかどうかを確認することについて説明します。

## 方法
私たちは、Elmでディレクトリが存在するかどうかをチェックするためにどのようにコードを書くことができるかを見ていきます。

```Elm
-- ディレクトリを確認するための関数
checkDirectory : String -> Cmd Msg
checkDirectory path =
  Cmd.map DirectoryChecked (Directory.exists path)

-- メッセージの処理
type Msg
  = DirectoryChecked Bool
  
-- サンプルで使用するパス
samplePath : String
samplePath = "/my/directory/"

-- チェックコマンドを実行
CheckDirectory samplePath
```

上記のコードでは、`checkDirectory`関数を使用して指定したパスのディレクトリが存在するかどうかをチェックし、`DirectoryChecked`メッセージを返します。`samplePath`を使用して`checkDirectory`関数にパスを渡し、`CheckDirectory`コマンドを実行することでテストすることができます。

```Elm
module Main exposing (..)

import Directory
  exposing (exists)


main : Program Never Model Msg
main =
  program 
    { view = view
    , update = update
    , subscriptions = subscriptions
    }

-- View
view : Model -> Html Msg
view model =
  div [] 
    [ button [ onClick (CheckDirectory samplePath) ] [ text "ディレクトリを確認する" ]
    , div [] [ text (if model.directoryExists then "ディレクトリが存在します" else "ディレクトリが存在しません") ]
    ]

-- Update
type Msg
  = CheckDirectory String
  | DirectoryChecked Bool
  

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CheckDirectory path ->
      ( model, checkDirectory path )
      
    DirectoryChecked exists ->
      ( { model | directoryExists = exists }
      , Cmd.none
      )


-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- Model
type alias Model = 
  { directoryExists : Bool
  }

initialModel : Model
initialModel = 
  { directoryExists = False
  }
  
-- Sample path
samplePath : String
samplePath = "/my/directory/"
```

上記のコードを実行すると、ボタンをクリックすることで指定したパスのディレクトリが存在するかどうかが判断され、その結果が表示されます。

## ディープダイブ
ディレクトリをチェックする方法は実はとてもシンプルです。Elmの標準ライブラリである`Directory`モジュールには`exists`関数が用意されており、引数にチェックしたいパスを与えることでディレクトリの存在を判断することができます。

また、`exists`関数の戻り値は`Cmd Bool`であり、ディレクトリの存在を確認するためだけに使用されることができます。このため、コードの見通しを良くするために上記のように`DirectoryChecked`メッセージを定義しています。そのため、結果を返す必要がある場合でも`exists`関数を直接使用しても問題ありません。

## その他の参考資料
- [Elm Directory.exists documentation](https://package.elm-lang.org/packages/elm/core/latest/Directory#exists)
- [Directory commands example](https://ellie-app.com/jYM5QrjEM3Ha1)
- [Elm for Beginners tutorial](https://elmprogramming.com/elm-for-beginners/introduction.html) See Also
  参考リンク：
  
  [Elmのはじめ方](https://elmprogramming.com/elm-for-beginners/int