---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストの送信とは、ウェブサーバーに情報をリクエストする仕組みのことで、共有データの取得や操作が可能となります。プログラマーはAPIにアクセスしたり、他のウェブサービスと通信したりするためにこの手段を利用します。

## 方法：

PHPにはもともとHTTPリクエストを送るための関数が組み込まれています。以下はその例です。

```PHP
<?php
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, 'http://example.com');
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
$output = curl_exec($ch);
curl_close($ch);
print_r($output);
?>
```

## 深堀り：

HTTPリクエストの送信は、ウェブが誕生した1991年から存在します。それ以来、様々なプログラミング言語やライブラリが、この基本的なクライアントとサーバー間の通信の規則を支えてきました。

また代替手段として、`file_get_contents`や`http_get`などのPHP組み込み関数を利用する方法もありますが、これらは一部の限定的なシーンでしか使えないこと、HTTPヘッダー情報が制限されることがデメリットとなります。

実装詳細についてですが、個々のcurl機能はlibcurlというバックエンドライブラリによって可能となっています。具体的には、`curl_init` でセッションを初期化、`curl_setopt`でオプションを設定、最後に`curl_exec`で実行します。

## 参考：

- [PHP: クライアントURLライブラリ - Manual](https://www.php.net/manual/ja/book.curl.php)
- [HTTPメッセージ - Web開発入門 | MDN](https://developer.mozilla.org/ja/docs/Web/HTTP/Messages)
- [Guzzle, PHP HTTP client](http://docs.guzzlephp.org/en/stable/) 

これらのリンクを参照して、PHPでのHTTPリクエスト送信についてさらに学びましょう。