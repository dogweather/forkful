---
date: 2024-01-26 01:18:09.501256-07:00
description: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u306F\u3001\u57FA\u672C\
  \u7684\u306B\u306F\u30B3\u30FC\u30C9\u30D9\u30FC\u30B9\u306E\u5927\u6383\u9664\u3067\
  \u3059\u3002\u305D\u308C\u306F\u65E2\u5B58\u306E\u30B3\u30FC\u30C9\u306E\u69CB\u9020\
  \u3092\u5909\u3048\u308B\u3053\u3068\u3067\u3001\u305D\u306E\u5916\u90E8\u52D5\u4F5C\
  \u3092\u5909\u3048\u305A\u306B\u884C\u3044\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u3092\u3088\u308A\u8AAD\u307F\u3084\u3059\
  \u304F\u3057\u3001\u8907\u96D1\u3055\u3092\u6E1B\u3089\u3057\u3001\u4FDD\u5B88\u6027\
  \u3092\u5411\u4E0A\u3055\u305B\u3001\u62E1\u5F35\u3057\u3084\u3059\u304F\u3059\u308B\
  \u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.019726-06:00'
model: gpt-4-0125-preview
summary: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u306F\u3001\u57FA\u672C\
  \u7684\u306B\u306F\u30B3\u30FC\u30C9\u30D9\u30FC\u30B9\u306E\u5927\u6383\u9664\u3067\
  \u3059\u3002\u305D\u308C\u306F\u65E2\u5B58\u306E\u30B3\u30FC\u30C9\u306E\u69CB\u9020\
  \u3092\u5909\u3048\u308B\u3053\u3068\u3067\u3001\u305D\u306E\u5916\u90E8\u52D5\u4F5C\
  \u3092\u5909\u3048\u305A\u306B\u884C\u3044\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u3092\u3088\u308A\u8AAD\u307F\u3084\u3059\
  \u304F\u3057\u3001\u8907\u96D1\u3055\u3092\u6E1B\u3089\u3057\u3001\u4FDD\u5B88\u6027\
  \u3092\u5411\u4E0A\u3055\u305B\u3001\u62E1\u5F35\u3057\u3084\u3059\u304F\u3059\u308B\
  \u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
weight: 19
---

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
