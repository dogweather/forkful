---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となんで？

HTTPリクエストでの基本認証を送信するというのは、ユーザー名とパスワードを通じてのリモートサーバーへの認証であり、APIのようなプライベートリソースへのアクセス許可を取得するために、プログラマーが行う必要がある。

## どうするか？

PHP cURLを用いて、基本なHTTP認証を送る方法を見てみましょう。

```PHP
<?php
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL, "http://your-website.com/api");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "username:password");

$result = curl_exec($ch);

if($result === false)
{
    echo 'エラー: ' . curl_error($ch);
}
else
{
    echo $result;
}

curl_close($ch);
?>
```
この短いコードが実行されると、指定のURLに対して基本的なHTTP認証を使用してGETリクエストが送信されます。これにより、サーバーからの応答は変数$result に保存されます。

## 深堀り

cURLが始まったのは遠く1997年のことで、その役割はHTTP、FTP、SMTPなどのプロトコルによるデータ送信をサポートすることです。現在では、ほとんどのサーバーサイドプログラミングにおいて、HTTPリクエストの送信に使われます。

しかし、PHP自体も基本認証を送信するための代替手段を提供します。一つは`file_get_contents`と`stream_context_create`で、これを使うことで基本認証を含むリクエストを送信できます。しかし、この方法はcURLほど高度に機能しません。HTTP認証だけではなく、プロキシ、クッキー、セッション、ヘッダー操作など、より多機能なニーズに応えたい場合はcURLの利用が推奨されます。

## 参考資料

以下は、HTTP認証及びcURLについての有用なリンクです：

- [PHP公式のcURLチュートリアル](http://php.net/manual/en/book.curl.php)
- [HTTP認証 - MDN](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)
- [cURL 公式ドキュメンテーション](https://curl.haxx.se/docs/)