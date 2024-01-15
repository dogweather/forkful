---
title:                "ウェブページのダウンロード"
html_title:           "Haskell: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ダウンロードする理由

Webページをダウンロードすることで、ウェブコンテンツをオフラインで閲覧したり、ブログやニュースサイトの記事を保存したりすることができます。また、自分のプログラムやアプリケーションの一部としてウェブスクレイピングを用いることもできます。

## ダウンロードの方法

まず、必要なライブラリをインポートします。

```Haskell
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
```

次に、`httpSimpelRequest`を使ってウェブサイトのURLを与えてリクエストを作成します。

```Haskell
request <- parseRequest "https://example.com"
```

リクエストを実行してレスポンスを得るには、`httpLBS`関数を使います。

```Haskell
response <- httpLBS request
```

レスポンスからコンテンツを抽出するためには、`getResponseBody`関数を使います。

```Haskell
body <- getResponseBody response
```

抽出したコンテンツはバイト文字列として返されるため、必要に応じて文字列に変換することができます。

```Haskell
let content = L8.unpack body
```

## 詳細を深く掘り下げる

ウェブページをダウンロードするにはさまざまな方法があります。例えば、`httpSimpelRequest`の代わりに`httpSimpelRequestBS`を使うことで、レスポンスをバイト文字列ではなくテキストとして直接受け取ることができます。また、パラメータを追加することで、リクエストをカスタマイズすることもできます。詳細については、Haskellのドキュメントを参照してください。

## 他にも参考になる記事

- [Haskellで簡単なウェブスクレイピング](https://qiita.com/yuyakato/items/7c3f1f1693fb8e0df803)
- [HaskellのHTTP Simpleパッケージのドキュメント](https://hackage.haskell.org/package/http-client)
- [Haskellでウェブスクレイピングをする方法](https://qiita.com/emergent/items/9fd00405254468c09801)