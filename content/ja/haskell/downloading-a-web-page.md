---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何となぜ？

ウェブページのダウンロードとは、特定のウェブページの内容を自分のコンピュータに保存することです。プログラマーがこれを行う主な理由は、データの分析やアプリ内にコンテンツを取り込むためです。

## 使い方

HaskellのHTTPクライアントライブラリである`wreq`を使用してウェブページをダウンロードしてみましょう。

```Haskell
import Network.Wreq

-- URLから内容を取得
download :: String -> IO String
download url = do
    r <- get url
    return (r ^. responseBody . to L.toString)
```

上記のコードを使用すると、指定したURLのウェブページの内容を取得できます。

```Haskell
-- コードを試す
main :: IO ()
main = do
    content <- download "http://example.com"
    print content
```

上記のコードを実行すると、http://example.comの内容が表示されます。

## 深掘り

ウェブページのダウンロードは、インターネットが誕生した初期から行われていました。元々は単純なテキストファイルでしたが、今日ではHTML, CSS, JavaScriptなどの複雑な形式が使われています。

`wreq`の代わりに`http-conduit`や`http-client`などの他のライブラリを使用することも可能です。各ライブラリには利点と欠点がありますので、プロジェクトの特定のニーズに基づいて最善の選択をすることが重要です。

このコードの背後では、HTTP GETリクエストが送信され、レスポンスのボディが解析されています。複雑なユースケースでは、エラーハンドリングやリダイレクトの処理、さらには認証まで行うことがあります。

## 参考資料

以下は、今回のテーマに関連する資料へのリンクです。

- [Haskell `wreq`ライブラリのドキュメンテーション](http://www.haskell.org/haddock/doc/html/Wreq.html)
- [ウェブスクレイピングのためのHaskellライブラリ](https://github.com/sras/scrapegood)
- [HTTPリクエストとレスポンスについての詳細な説明](https://developer.mozilla.org/ja/docs/Web/HTTP/Messages)