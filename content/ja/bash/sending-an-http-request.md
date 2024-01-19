---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストを送信するとは、サーバーに情報を要求または送信するプロセスを指します。プログラマーはAPIの情報を取得したり、外部サーバーと通信したりするためにこれを行います。

## 方法:

```Bash
# cURL を使って HTTP GET リクエストを送信します
curl https://example.com

# cURL を使って HTTP POST リクエストを送信します。データは JSON 形式で送られます
curl -X POST -H "Content-Type: application/json" -d '{"name":"Yamada", "job":"programmer"}' https://example.com/api/users

# レスポンス：
#  {
#    "name": "Yamada",
#    "job": "programmer",
#    "id": "123",
#    "createdAt": "2019-03-15T15:24:00.000Z"
#  }
```

## 深掘り:

HTTPリクエストの送信は、初期のインターネットの頃から存在しています。それは、Webページへのアクセス、ファイルのダウンロード、ユーザー情報の送受信など、多くの目的に使用されます。

代替手段として、`wget` コマンドもありますが、cURLの方がオプションが豊富で多機能です。

HTTPリクエストの送信は、基本的にはクライアントからサーバーへのメッセージ送信です。しかし、通常、このメッセージにはヘッダ情報（メタデータや設定情報）とボディ（送信するデータ本体）が含まれます。

## 参考情報:

- cURL 公式ドキュメント: [https://curl.haxx.se/](https://curl.haxx.se/)
- HTTPについての詳細: [https://developer.mozilla.org/ja/docs/Web/HTTP/Overview](https://developer.mozilla.org/ja/docs/Web/HTTP/Overview)
- `wget` について: [https://www.gnu.org/software/wget/](https://www.gnu.org/software/wget/)