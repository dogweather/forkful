---
title:                "HTTPリクエストの送信"
date:                  2024-01-20T18:00:05.324690-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/sending-an-http-request.md"
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
