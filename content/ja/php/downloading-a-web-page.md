---
title:                "ウェブページのダウンロード"
html_title:           "PHP: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

Webページのダウンロードへの参加には、ウェブサイト上の情報を取得し、処理して利用する必要があるため、ダウンロードすることが重要です。

## 方法

```PHP
<?php

$url = "https://example.com"; // ダウンロードするWebページのURLを指定

// cURLを使用してWebページをダウンロード
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$page = curl_exec($ch);
curl_close($ch);

// ダウンロードしたページの内容を表示
echo $page;
```

ダウンロードしたWebページの内容を取得し、PHPの変数に保存して表示することができます。

## 詳細を深く調べる

Webページをダウンロードする方法はいくつかありますが、PHPのcURLライブラリを使用することが一般的です。cURLは、Webサーバーとの通信を行うための多目的なライブラリであり、ダウンロードしたページの内容を自由に処理することができます。また、オプションを設定することで、認証やリダイレクトの処理なども行うことができます。

## 参考リンク

* [PHP: cURLライブラリ](https://www.php.net/manual/ja/book.curl.php)
* [cURLを使ってWebページをダウンロードする方法](https://www.php.net/manual/ja/function.curl-init.php)