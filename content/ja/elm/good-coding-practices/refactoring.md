---
title:                "リファクタリング"
date:                  2024-01-26T01:18:09.501256-07:00
model:                 gpt-4-0125-preview
simple_title:         "リファクタリング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/refactoring.md"
---

{{< edit_this_page >}}

## 何となぜ？
リファクタリングは、基本的にはコードベースの大掃除です。それは既存のコードの構造を変えることで、その外部動作を変えずに行います。プログラマーは、コードをより読みやすくし、複雑さを減らし、保守性を向上させ、拡張しやすくするためにこれを行います。

## 方法：
たとえば、UIロジックと状態更新が混在しているような、多くのことを行っているElm関数があるとします。それはリファクタリングのための完璧な候補です。元々は：

```Elm
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    let
        updatedModel = { model | userInput = input }
    in
    if String.length input > 5 then
        ( updatedModel, Cmd.none )
    else
        ( model, Cmd.none )
```

リファクタリング後、異なる関数にロジックを引き出すことで関心の分離を行います：

```Elm
-- 更新ロジックは別になっています
updateUserInput : String -> Model -> Model
updateUserInput input model = 
    { model | userInput = input }

-- フォーマッティング（ビュー）ロジックも別になっています
formatUserInput : Model -> (Model, Cmd Msg)
formatUserInput model =
    if String.length model.userInput > 5 then
        ( model, Cmd.none )
    else
        ( { model | userInput = "" }, Cmd.none ) -- 例として、入力が短すぎる場合はクリアします。

-- 更新機能は今ヘルパー関数を使用しています
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    model
    |> updateUserInput input
    |> formatUserInput
```
これらの変更により、明確な分離が行われ、各関数が理解しやすく、テストしやすくなります。

## ディープダイブ
リファクタリングとしての公式な実践は、コードの変更のコストが開発プロセスの重要な側面としてすでに認識されていたプログラミングの初期の日々までさかのぼることができます。特に、1990年代後半に出版されたマーティン・ファウラーの著書「Refactoring: Improving the Design of Existing Code」は、構造化されたアプローチと「コードの臭い」と呼ばれるリファクタリングの機会を特定するカタログでリファクタリングの舞台を設定しました。

Elmの文脈では、リファクタリングはプロセス中に信頼を促進する強力な型システムのような、言語の強みを活用しています。手動リファクタリングへの代替手段には、自動化されたコード変換ツールも含まれますが、Elmのツールは一部の古い言語に比べてまだ成熟している段階です。実装の詳細は、関数の抽出、名前の変更、条件の簡素化などの一般的なリファクタリングを中心に展開されます。Elmのコンパイラはリファクタリングにおいて重要な味方であり、多くを許してくれません。何か問題があるとすぐに警告し、リファクタリングされたコードがまだ動作することを保証します。

## 参考文献
- ["Refactoring: Improving the Design of Existing Code" by Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [Elm Discourse - Refactoringに関するトピック](https://discourse.elm-lang.org/search?q=refactoring)
