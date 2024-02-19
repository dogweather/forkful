---
aliases:
- /ja/php/sending-an-http-request/
date: 2024-01-20 18:00:05.324690-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\u3044\
  \u3046\u306E\u306F\u3001Web\u30B5\u30FC\u30D0\u30FC\u306B\u30C7\u30FC\u30BF\u3092\
  \u53D6\u5F97\u3001\u9001\u4FE1\u3001\u5909\u66F4\u3092\u4F9D\u983C\u3059\u308B\u3053\
  \u3068\u3002\u3053\u306E\u6280\u8853\u306F\u3001API\u304B\u3089\u60C5\u5831\u3092\
  \u53D6\u5F97\u3057\u305F\u308A\u3001\u30D5\u30A9\u30FC\u30E0\u3092\u30B5\u30D6\u30DF\
  \u30C3\u30C8\u3059\u308B\u3068\u304D\u306A\u3069\u306B\u4F7F\u308F\u308C\u308B\u3088\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.992537
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\u3044\u3046\
  \u306E\u306F\u3001Web\u30B5\u30FC\u30D0\u30FC\u306B\u30C7\u30FC\u30BF\u3092\u53D6\
  \u5F97\u3001\u9001\u4FE1\u3001\u5909\u66F4\u3092\u4F9D\u983C\u3059\u308B\u3053\u3068\
  \u3002\u3053\u306E\u6280\u8853\u306F\u3001API\u304B\u3089\u60C5\u5831\u3092\u53D6\
  \u5F97\u3057\u305F\u308A\u3001\u30D5\u30A9\u30FC\u30E0\u3092\u30B5\u30D6\u30DF\u30C3\
  \u30C8\u3059\u308B\u3068\u304D\u306A\u3069\u306B\u4F7F\u308F\u308C\u308B\u3088\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why? (何とその理由?)
HTTPリクエストを送るっていうのは、Webサーバーにデータを取得、送信、変更を依頼すること。この技術は、APIから情報を取得したり、フォームをサブミットするときなどに使われるよ。

## How to: (方法:)
PHPの`curl`を使ってみよう。シンプルなGETリクエストから。

```php
<?php
$curl = curl_init();

curl_setopt($curl, CURLOPT_URL, "http://example.com"); // URLをセット
curl_setopt($curl, CURLOPT_RETURNTRANSFER, 1); // 結果を文字列として返す

$response = curl_exec($curl); // HTTPリクエストを実行

if ($response === false) {
    echo 'Curl error: ' . curl_error($curl);
} else {
    echo 'Response: ' . $response;
}

curl_close($curl); // セッションを終了
?>
```

これで、`http://example.com` から取得した内容が表示されるはず。

## Deep Dive (深掘り:)
HTTPリクエストはインターネットの中核的な部分。1990年代初頭のWeb誕生以来使われてる。PHPでHTTPリクエストを送る方法はいくつかあるが、`curl`が最も一般的。`file_get_contents()`や、新しい`HTTP context`オプション、`Guzzle`といったライブラリも選択肢に入る。`curl`は直接的で強力な機能を多く持ち、カスタムヘッダやPOSTデータの送信、認証といった高度な操作もこなせる。

## See Also (関連情報):
- PHP Manual on cURL: [https://www.php.net/manual/ja/book.curl.php](https://www.php.net/manual/ja/book.curl.php)
- Guzzle, PHP HTTP client: [http://docs.guzzlephp.org/en/stable/](http://docs.guzzlephp.org/en/stable/)
- PHP streams, for people who prefer not using cURL: [https://www.php.net/manual/ja/book.stream.php](https://www.php.net/manual/ja/book.stream.php)
