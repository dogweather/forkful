---
date: 2024-01-20 18:00:05.324690-07:00
description: "How to: (\u65B9\u6CD5:) PHP\u306E`curl`\u3092\u4F7F\u3063\u3066\u307F\
  \u3088\u3046\u3002\u30B7\u30F3\u30D7\u30EB\u306AGET\u30EA\u30AF\u30A8\u30B9\u30C8\
  \u304B\u3089\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.241010-06:00'
model: gpt-4-1106-preview
summary: "PHP\u306E`curl`\u3092\u4F7F\u3063\u3066\u307F\u3088\u3046\u3002\u30B7\u30F3\
  \u30D7\u30EB\u306AGET\u30EA\u30AF\u30A8\u30B9\u30C8\u304B\u3089."
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
