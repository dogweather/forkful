---
title:                "Elm: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ？

Webページをダウンロードする理由はさまざまです。たとえば、データを抽出するためにスクレイピングを行う、オフラインで閲覧するためにページを保存する、あるいは単純にページをバックアップするためにダウンロードするかもしれません。

## 使い方

Elmプログラミング言語を使用してWebページをダウンロードする方法はとても簡単です。まず、`Http`モジュールをインポートします。次に、ダウンロードしたいページのURLを指定し、`send`関数を使用してリクエストを送信します。最後に、取得したレスポンスを解析して必要なデータを取得します。

```Elm
import Http

Http.send
    { url = "https://www.example.com"
    , expect = Http.expectString handleResponse
    }

handleResponse : Result Http.Error String -> msg
handleResponse result =
    case result of
        Ok body ->
            -- 解析して必要なデータを取得する

        Err error ->
            -- エラー処理を行う
```

## ディープダイブ

Webページをダウンロードする方法にはいくつかのオプションがあります。たとえば、リクエストヘッダーをカスタマイズすることや、ダウンロード速度を管理するためにチャンクを使用することができます。また、コマンドライン引数からURLを受け取ってダウンロードするスクリプトを作成することもできます。

## 参考リンク

- [Elmの公式ドキュメント](https://guide.elm-lang.jp/)
- [httpパッケージのドキュメント](https://package.elm-lang.org/packages/elm/http/latest/)
- [Webスクレイピングのチュートリアル（英語）](https://dev.to/rjmurtagh/scraping-the-web-with-elm-2o7d)