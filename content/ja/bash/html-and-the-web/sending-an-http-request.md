---
date: 2024-01-20 17:59:31.687551-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3068\u306F\u3001\
  Web\u30B5\u30FC\u30D0\u30FC\u306B\u60C5\u5831\u3092\u8981\u6C42\u3059\u308B\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\
  \u30BF\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001Web\u30B5\u30A4\u30C8\u306E\u72B6\
  \u614B\u3092\u78BA\u8A8D\u3057\u305F\u308A\u3001API\u3092\u4ECB\u3057\u3066\u64CD\
  \u4F5C\u3092\u5B9F\u884C\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.365574-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3068\u306F\u3001Web\u30B5\
  \u30FC\u30D0\u30FC\u306B\u60C5\u5831\u3092\u8981\u6C42\u3059\u308B\u3053\u3068\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u3092\
  \u53D6\u5F97\u3057\u305F\u308A\u3001Web\u30B5\u30A4\u30C8\u306E\u72B6\u614B\u3092\
  \u78BA\u8A8D\u3057\u305F\u308A\u3001API\u3092\u4ECB\u3057\u3066\u64CD\u4F5C\u3092\
  \u5B9F\u884C\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
