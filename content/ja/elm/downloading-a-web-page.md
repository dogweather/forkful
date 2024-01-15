---
title:                "ウェブページのダウンロード"
html_title:           "Elm: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

ウェブページのダウンロードをする理由は様々ですが、例えばオフラインで閲覧したい場合や、コンテンツの更新をチェックするために使用することができます。

## ダウンロードの方法

```Elm
import Html exposing (text)
import Http

main =
    Http.get "www.example.com" -- ダウンロードしたいページのURLを指定
        |> Http.send
        |> Task.attempt handleResponse
```

このコードは`Html.text`をインポートし、`Http`を使って指定したURLからページをダウンロードする方法を示しています。`Task.attempt`という関数を使って、ダウンロードが完了した際に`handleResponse`という関数を実行します。ダウンロードが成功した場合、`handleResponse`はダウンロードされたページのデータを引数として受け取り、欲しいコンテンツを取り出すことができます。

## 詳細を掘り下げる

ウェブページのダウンロードはElmの`Http`モジュールを使って行うことができます。`Http.get`を使って`GET`リクエストを送信し、ダウンロードしたいページのURLを指定します。ダウンロードが成功した場合、受け取ることができるデータの種類は`Http.expectString`や`Http.expectJson`を使って指定することができます。また、ダウンロードの進捗状況を監視するために`Http.sendProgress`を使用することもできます。

## 併せて参考にしてください

- [Elm Lang 公式ドキュメント](https://elm-lang.org/)
- [Elm in Action](https://www.manning.com/books/elm-in-action)
- [Elm Community](https://discourse.elm-lang.org/)