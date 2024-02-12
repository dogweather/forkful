---
title:                "HTTPリクエストの送信"
aliases:
- /ja/bash/sending-an-http-request/
date:                  2024-01-20T17:59:31.687551-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストを送るとは、Webサーバーに情報を要求することです。プログラマーは、データを取得したり、Webサイトの状態を確認したり、APIを介して操作を実行するためにこれを行います。

## How to: (方法)
Bashを使って、HTTPリクエストを手軽に送るためには、`curl` コマンドがよく使われます。以下、基本的な例を見てみましょう。

```Bash
# GETリクエストでウェブページを取得
curl http://example.com

# POSTリクエストでデータを送信
curl -d "param1=value1&param2=value2" -X POST http://example.com/resource

# ヘッダーを追加する
curl -H "Content-Type: application/json" -d '{"key1":"value1", "key2":"value2"}' http://example.com/resource

# レスポンスヘッダーを表示
curl -i http://example.com
```

各コマンドを実行すると、サーバーからのレスポンスがコンソールに表示されます。

## Deep Dive (深掘り)
HTTPリクエストを送る機能は、1990年のWWWの発足以来、Webの基本です。BashからHTTPリクエストを送れる`curl`は、Daniel Stenberg によって1997年にリリースされました。`wget`やライブラリを使った方法（`HTTPie`や`wget`）、より進んだスクリプトで使うためのツール（`httpx`や`batsh`など）もありますが、シンプルさと普及度から`curl`が最も頻繁に使われます。

`curl`は多機能で、HTTPだけでなく、FTPやSMTPなど他のプロトコルにも対応しています。リクエストのカスタマイズ、アップロードとダウンロードの管理、シンプルなREST APIのテストなど様々な用途に使われています。

HTTPSを介したセキュアなリクエスト送信には、`-k` オプションや適切な CA 証明書を指定する方法があります。複雑な操作を必要とする場合、Bashスクリプト内に`curl`コマンドを組み込んで自動化することも可能です。

## See Also (関連情報)
- cURL公式サイト: [https://curl.se](https://curl.se)
- HTTPリクエストに関するWikipediaのページ: [https://ja.wikipedia.org/wiki/HTTP](https://ja.wikipedia.org/wiki/HTTP)
- Bashスクリプトについての詳細: [https://www.gnu.org/software/bash/manual/](https://www.gnu.org/software/bash/manual/)
- HTTPie: [https://httpie.io/](https://httpie.io/)
- HTTPクライアントとしてのwget: [https://www.gnu.org/software/wget/](https://www.gnu.org/software/wget/)
