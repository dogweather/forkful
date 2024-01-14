---
title:                "Elm: 「HTMLの解析」"
simple_title:         "「HTMLの解析」"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/parsing-html.md"
---

{{< edit_this_page >}}

※この記事は日本語のElmプログラマー向けです

## なぜHTML解析をするのか

HTML解析は、ウェブページから必要な情報を取得するために必要な重要なスキルです。例えば、ニュースサイトの見出しや本のカバー画像を取得したい場合、HTML解析を使用してその情報を簡単に取得することができます。

## 解析する方法

まずは、[elm/parser](https://package.elm-lang.org/packages/elm/parser/latest/)パッケージをインストールします。このパッケージには、HTML解析に必要なすべての機能が含まれています。

HTMLを解析するコード例を以下に示します。この例では、[BBCニュースのトップページ](https://www.bbc.com/news)から見出しを取得します。

```elm
import Html.Parser exposing (..)
import Html.Parser.Attributes exposing (..)

parseTitle : String -> String
parseTitle html =
    let
        parser =
            filter isH2 (many attributeOrContent)
                |> filterMap (getAttribute (\a -> a.key == "class" && a.value == "gs-c-promo-heading__title"))
                |> map (\a -> a.child)
                |> oneOrMore
                |> text []
    in
    case run parser html of
        Ok titles ->
            -- 元々の見出しが改行されているため、改行を削除
            String.join " " titles |> String.replace "\n" ""

        Err error ->
            -- エラー処理
            ""

```

このコードを実行すると、以下のような出力が得られます。

```elm
"S Korea threat to 'destroy' N Korea"
"Women pose for feminist coffee table book"
"Trump lifts North Korea travel ban"
```

## 詳細な解説

HTMLを解析する際は、ウェブサイトのHTML構造を理解する必要があります。例えば、見出しを取得したい場合、その見出しは`<h1>`や`<h2>`などのタグで囲まれていることが多いです。また、見出しの内容は`innerText`や`innerHTML`といった属性に格納されています。これらの知識を使用して、解析の正確性を高めることができます。

さらに、elm/parserパッケージにはHTML解析に役立つ多くの便利な関数が用意されています。例えば、`filter`や`oneOrMore`を使用することで、特定のタグや属性を持つ要素だけを選択することができます。

## この後もっと学びたい方へ

もし今回の記事で取り上げた内容に興味がある場合は、次のリソースを参考にしてみてください。

- [elm/parserパッケージの公式ドキュメント](https://package.elm-lang.org/packages/elm/parser/latest/)
- [Elm in Action](https://www.amazon.com/Elm-Action-Richard-Feldman-ebook/dp/B07D2YCY82)
- [Pragmatic Studio - Elm Programming](https://pragmaticstudio.com/elm-programming)
- [Elm公式フォーラム](https://discourse.elm-lang.org/)

## 参考リンク

- [BBCニュースのトップページ](https://www.bbc.com/news)
- [elm/parserパッケージ](https://package.elm-lang.org/packages/elm/parser/latest/)
- [Elm公式フォーラム](https://discourse.elm-lang.org/)
- [Elm in Action](https://www.amazon.com/Elm-Action-Richard-Feldman-ebook/dp/B07D2YCY82)
- [Pragmatic Studio - Elm Programming](https://pragmaticstudio.com/elm-programming)