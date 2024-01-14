---
title:                "Haskell: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

ウェブページをダウンロードする理由は様々ですが、例えば情報を収集したいときや、特定のデータを入手したいときに必要となります。

## 方法

ウェブページをダウンロードするためには、最初にHTTPリクエストを送信し、レスポンスを受け取る必要があります。HaskellでHTTPリクエストを送信するには、`Network.HTTP.Simple`モジュールを使用します。

まず、必要なモジュールをインポートします。

```Haskell
import Network.HTTP.Simple
```

次に、ダウンロードしたいウェブページのURLを指定し、`httpSimple`関数を使用してHTTPリクエストを送信します。

```Haskell
let url = "https://example.com"
response <- httpSimple url
```

このコードを実行すると、ウェブページからのレスポンスが`response`変数に格納されます。

もし、ウェブページからのレスポンスがJSON形式であれば、`httpJSON`関数を使用してレスポンスをJSONオブジェクトとして取得することができます。

```Haskell
response <- httpJSON url
```

このコードを実行すると、ウェブページのJSONデータが`response`変数に格納されます。

## 深堀り

ウェブページをダウンロードする際には、通常はHTTPリクエストのヘッダーをカスタマイズする必要があります。Haskellでは、`Request`タイプを使用してHTTPリクエストの情報をカスタマイズすることができます。

例えば、ユーザーエージェントをカスタマイズする場合は、`Request`の`requestHeaders`フィールドにカスタムヘッダーを追加することができます。

```Haskell
import Network.HTTP.Types.Header

let userAgent = (hUserAgent, "My Custom User-Agent")
let request = setRequestHeaders [userAgent] $ setRequestHeader hAccept ["text/html"] $ defaultRequest { host = "example.com" }
```

このコードでは、`My Custom User-Agent`という値を持つ`User-Agent`ヘッダーを追加し、`text/html`という値を持つ`Accept`ヘッダーを指定したHTTPリクエストを作成しています。

## 参考リンク

- [Network.HTTP.Simpleモジュールのドキュメント](https://hackage.haskell.org/package/http-simple)
- [HTTPリクエストのカスタマイズ方法についてのガイド](https://dev.to/kris/understand-networkhttp-and-make-basic-http-requests-with-this-tutorial-3afl)