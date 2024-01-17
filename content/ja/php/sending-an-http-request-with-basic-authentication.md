---
title:                "基本認証を使用したhttpリクエストの送信"
html_title:           "PHP: 基本認証を使用したhttpリクエストの送信"
simple_title:         "基本認証を使用したhttpリクエストの送信"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

HTTPリクエストを基本認証付きで送信するとは、プログラマーがサーバー上で特権を取得してリソースにアクセスするための方法です。プログラマーは、この認証方法を使用することで、ユーザーのプライバシーを保護し、機密の情報を安全に送信することができます。

## 方法：

```PHP
$username = 'ユーザー名';
$password = 'パスワード';
$url = 'リクエストを送信するURL';

// 基本認証を使用してHTTPリクエストを送信する
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_USERPWD, $username . ":" . $password);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

// レスポンスを取得する
$response = curl_exec($ch);
curl_close($ch);

// 結果を出力する
echo $response;
```

```PHP
// Guzzleライブラリを使用してHTTPリクエストを送信する
$client = new GuzzleHttp\Client();
$res = $client->request('GET', $url, [
    'auth' => [$username, $password]
]);

// レスポンスを取得する
$response = $res->getBody();

// 結果を出力する
echo $response;
```

## 詳細：

基本認証は、HTTPの最も古いセキュリティプロトコルの一つです。これは、サーバーにアクセスするときにユーザー名とパスワードを提供することで認証を行います。しかし、最近では基本認証を使用することは推奨されません。代わりに、トークン認証やOAuthなどのより安全な認証方法が使用されています。

HTTPリクエストを送信する方法はさまざまありますが、PHPではcURLやGuzzleなどのライブラリを使用することで簡単に実装することができます。これらのライブラリは、ネットワークリクエストを作成し、レスポンスを受け取るための便利な機能を提供しています。

## 関連リンク：

- [cURLドキュメンテーション](https://www.php.net/manual/en/book.curl.php)
- [Guzzleドキュメンテーション](https://docs.guzzlephp.org/en/stable/)
- [基本認証についての詳細](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)