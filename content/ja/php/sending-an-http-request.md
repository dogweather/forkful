---
title:                "「HTTPリクエストの送信」"
html_title:           "PHP: 「HTTPリクエストの送信」"
simple_title:         "「HTTPリクエストの送信」"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
HTTPリクエストを送信することとは何か、そしてプログラマーがそれを行う理由を説明します。
## プロセス
下記のコードブロック内にあるコーディング例とサンプル出力を参照してください。

```PHP
// cURLを使用してHTTPリクエストを送信する方法
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, "https://example.com/api");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$response = curl_exec($ch);
curl_close($ch);

// Guzzleを使用してHTTPリクエストを送信する方法
$client = new GuzzleHttp\Client();
$response = $client->request('GET', 'https://example.com/api')->getBody();
```

## さらに詳しく
HTTPリクエストを送信するプロセスについての歴史的背景や、他の代替方法、また実装の詳細について詳しく説明します。
HTTPリクエストを送信する方法には、cURLやGuzzleのようなライブラリを使用する方法が一般的です。これらのライブラリはHTTP通信を簡単に行うことができ、多くのオプションを提供しています。しかし、PHPには標準で用意されている`fopen`や`file_get_contents`といった関数を使用する方法もあります。これらの関数は簡単に使用できますが、オプションの設定はできません。どの方法を使用するかは、目的やプロジェクトの仕様によって異なります。

## 関連リンク
- [cURLの公式ドキュメント](https://curl.haxx.se/docs/)
- [Guzzleの公式ドキュメント](https://docs.guzzlephp.org/en/stable/)
- [PHPマニュアル - fopen関数](https://www.php.net/manual/ja/function.fopen.php)
- [PHPマニュアル - file_get_contents関数](https://www.php.net/manual/ja/function.file-get-contents.php)