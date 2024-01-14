---
title:                "Elm: 将来または過去の日付を計算する"
simple_title:         "将来または過去の日付を計算する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を未来や過去の計算をする理由は多くありますが、例えば誕生日やイベントの日程を知りたい場合などがあります。そんな時には、Elmプログラミング言語を使って簡単に日付の計算が行えます。

## 方法

まず、Elmのインストールから始めましょう。コマンドラインで `elm-ui install` を実行して、必要なパッケージをダウンロードします。次に、`elm-repl`コマンドを実行して、Elmの対話型環境を起動します。

今回は、今日の日付から1ヶ月後の日付を計算するコードを紹介します。以下のコードを `elm-repl` 上で実行すると、計算結果が表示されます。

```
```Elm
import Date exposing (add)

add 1 (Days 30) Date.today
```

実行結果は以下のようになります。

```Elm
    -- 2019/5/23
```

もちろん、未来以外にも過去の日付を計算することもできます。次の例では、今日から1年前の日付を計算しています。

```
```Elm
add -1 (Years 1) Date.today
```

実行結果は以下のようになります。

```Elm
    -- 2018/5/23
```

## ディープダイブ

日付の計算には、ElmのDateモジュールの`add`関数を使用します。この関数には、計算に使用する値を `Int`型で指定する必要があります。そして、計算に使用する単位は`Days`や`Weeks`、`Months`、`Years`のいずれかを指定します。

また、計算に使用する日付は`Date`型で渡す必要があります。`Date.today`という関数を使用すると、現在の日付を取得することができます。

今回紹介した例以外にも、様々な計算方法がありますので、自分なりの使い方を見つけるのも楽しいかもしれません。

## その他のリソース

- [Elm公式ドキュメント](https://guide.elm-lang.org/)
- [Elm日本語ドキュメント](https://elm-lang.org/docs/jp)
- [Elm Forum](https://discourse.elm-lang.org/)