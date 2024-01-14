---
title:                "PHP: 基本認証付きのHTTPリクエストを送信する方法"
simple_title:         "基本認証付きのHTTPリクエストを送信する方法"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# なぜ
HTTPリクエストに基本認証を使って送信する理由について明確にするのは重要です。

# 方法
基本認証を使ったHTTPリクエストを送信する方法を理解するために、以下のコード例と出力を見てみましょう。

```PHP
<?php

// URLを指定
$url = "https://example.com/api";

// ユーザー名とパスワードを指定
$username = "username";
$password = "password";

// CURLを初期化
$curl = curl_init($url);

// リクエストに基本認証を追加
curl_setopt($curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($curl, CURLOPT_USERPWD, "$username:$password");

// リクエストを実行し、レスポンスを取得
$response = curl_exec($curl);

// レスポンスの状態コードを取得
$status = curl_getinfo($curl, CURLINFO_HTTP_CODE);

// リクエストを閉じる
curl_close($curl);

// 結果を出力
echo "ステータスコード: " . $status . "\n";
echo "レスポンス: " . $response;

?>
```

出力:

```
ステータスコード: 200
レスポンス: {"message": "認証に成功しました！"}
```

# 深堀り
基本認証は、WebサービスやAPIなどのセキュリティを強化するために使われる一般的な認証方法です。これを使うことで、ユーザー名とパスワードを使ってリクエストを送信する際に、よりセキュアにデータを保護することができます。また、基本認証はSSLやTLSよりも軽量であり、パフォーマンスが向上するというメリットもあります。

# 併せて参考になるリンク
- [PHP cURL公式ドキュメント](https://www.php.net/manual/en/book.curl.php)
- [HTTPベーシック認証の仕組みと使い方](https://www.ibm.com/support/knowledgecenter/ja/SS9H2Y_7.7.0/com.ibm.dp.doc/basic_auth.html)
- [HTTPSの仕組みとセキュリティについて](https://www.ipa.go.jp/security/awareness/vendor/programmingv2/contents/405.html)