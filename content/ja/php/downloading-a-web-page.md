---
title:                "ウェブページをダウンロードする"
html_title:           "PHP: ウェブページをダウンロードする"
simple_title:         "ウェブページをダウンロードする"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何をしているの？
ウェブページのダウンロードとは、ウェブ上のコンテンツを自分のコンピューターに取り込むことです。プログラマーがこれをする理由は、ウェブサイトから必要なデータを取得しアプリケーションに組み込むためです。

## 方法：
```PHP
<?php
// ウェブページのダウンロード
$page = file_get_contents('https://example.com');
// 画像ファイルのダウンロード
$image = file_get_contents('https://example.com/images/sampleimage.jpg');
// ダウンロードしたウェブページを表示
echo $page;
// ダウンロードした画像ファイルを保存
file_put_contents('sampleimage.jpg', $image);
?>
```

## 詳しく見る：
ウェブページのダウンロードは、ウェブの普及とともに生まれた機能の一つです。ウェブページを手軽に保存するために、ブラウザーには「保存」ボタンがありますが、プログラマーはこの機能を利用して、自分のコンピューターにデータを取得することができます。また、ファイル取得には```file_get_contents()```関数だけでなく、cURLやGuzzleなどのツールも利用できます。

## もっと詳しく：
- [PHP マニュアル - file_get_contents関数](https://www.php.net/manual/ja/function.file-get-contents.php)
- [cURL - 公式ドキュメント](https://curl.haxx.se/docs/)
- [Guzzle - 公式ドキュメント](http://docs.guzzlephp.org/en/stable/)