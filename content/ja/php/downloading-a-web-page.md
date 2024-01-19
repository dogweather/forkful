---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

### 何となぜ?

ウェブページをダウンロードするとは、特定のウェブページのデータを自分のコンピュータに保存することです。プログラマーがこれを行う理由の一つは、以降オフラインでそのデータにアクセスするため、またはさまざまな分析や操作を行うためです。

### 使い方：

以下は、PHPでウェブページをダウンロードする基本的な方法です：

```PHP
<?php
// file_get_contentsを用いたシンプルな例
$html = file_get_contents('http://example.com');

// 結果をファイルに保存
file_put_contents('/path/to/save.html',$html);
?>
```

上記のコードは、http://example.comのページをダウンロードし、そのHTMLを指定されたパスに保存します。

### 掘り下げ：

ウェブページのダウンロードはウェブスクレイピング（Web Scraping）の基本的な一部で、様々な形で利用されています。初期のインターネットでは、人々が各種ウェブサイトから情報を取得するための主要な手段でした。

代替策としてPythonのような他の言語もウェブスクレイピングに使われます。更に幅広い機能やライブラリが利用可能なことが特徴です。

PHPでウェブページをダウンロードするには`file_get_contents`関数を使用します。しかしこれには制限があり、例えば大きなファイルをダウンロードする場合やより複雑なHTTPリクエストを必要とする場合には、cURL等他の方法が推奨されます。

### 参考資料：

1. [PHP Documentation: file_get_contents](https://www.php.net/manual/ja/function.file-get-contents.php)
2. [PHP Documentation: cURL](https://www.php.net/manual/ja/book.curl.php)
3. [Python: BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)