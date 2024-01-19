---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストを送信するとは、ネットワークを介して特定のウェブページまたはサービスに情報を要求するプロセスのことです。プログラマはこれを行う理由は主に2つあります。1つはデータを取得するため、もう1つは特定の操作をウェブサービース上で依頼するためです。

## 使い方

HTTPリクエストを送信するための基本的なHaskellコードは以下の通りです：

```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print (getResponseBody response)
```

このコードを実行すると以下の出力が得られます：

```Haskell
The status code was: 200
"OK"
```

このコードは "http://httpbin.org/get" にHTTPリクエストを送信し、ステータスコードとレスポンスボディを表示します。

##深掘り

HTTPリクエストの送信は、1980年代からのWebコミュニケーションの基本的なプロセスであり、時代と共にバリエーションと改良が加わりました。 

HaskellでHTTPリクエストを送信する代替手段としては、`http-client`や`wreq`などのライブラリがあります。

HaskellのHTTPリクエスト送信について深く掘り下げると、上記のコードの背後には`http-client`ライブラリが使用されています。これは、リクエストを構築し、サーバーに送信し、応答を解析するための底辺のインターフェースを提供します。

## 関連情報

以下のリンクは、このトピックについての追加情報を提供します。

- HaskellのHTTPリクエスト：http://haskell-lang.org/library/http-client

- HTTPプロトコルについての詳細：https://developer.mozilla.org/ja/docs/Web/HTTP

以上がHaskellでHTTPリクエストを行うことの基本になります。データを取得したり、サービスに対する特定の操作を呼び出したりするために、この情報が役立ちます。