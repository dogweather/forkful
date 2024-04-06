---
date: 2024-01-20 17:59:31.687551-07:00
description: "How to: (\u65B9\u6CD5) Bash\u3092\u4F7F\u3063\u3066\u3001HTTP\u30EA\u30AF\
  \u30A8\u30B9\u30C8\u3092\u624B\u8EFD\u306B\u9001\u308B\u305F\u3081\u306B\u306F\u3001\
  `curl` \u30B3\u30DE\u30F3\u30C9\u304C\u3088\u304F\u4F7F\u308F\u308C\u307E\u3059\u3002\
  \u4EE5\u4E0B\u3001\u57FA\u672C\u7684\u306A\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\
  \u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.201798-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Bash\u3092\u4F7F\u3063\u3066\u3001HTTP\u30EA\u30AF\u30A8\u30B9\
  \u30C8\u3092\u624B\u8EFD\u306B\u9001\u308B\u305F\u3081\u306B\u306F\u3001`curl` \u30B3\
  \u30DE\u30F3\u30C9\u304C\u3088\u304F\u4F7F\u308F\u308C\u307E\u3059\u3002\u4EE5\u4E0B\
  \u3001\u57FA\u672C\u7684\u306A\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\
  \u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
