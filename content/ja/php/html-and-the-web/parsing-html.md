---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:05.177864-07:00
description: "PHP\u306B\u304A\u3051\u308BHTML\u306E\u30D1\u30FC\u30B9\u3068\u306F\u3001\
  HTML\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u304B\u3089\u7279\u5B9A\u306E\u60C5\u5831\
  \u3092\u62BD\u51FA\u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u3053\u306E\u4F5C\u696D\u3092\u884C\u3046\u306E\
  \u306F\u3001\u30C7\u30FC\u30BF\u62BD\u51FA\u306E\u81EA\u52D5\u5316\u3001\u30A6\u30A7\
  \u30D6\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3001\u307E\u305F\u306F\u69D8\u3005\
  \u306A\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\
  \u5F7C\u3089\u306E\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u3067\u7D71\
  \u5408\u3059\u308B\u305F\u3081\u3067\u3042\u308A\u3001\u624B\u52D5\u3067\u306E\u4ECB\
  \u5165\u306A\u3057\u306B\u6A5F\u80FD\u6027\u3092\u5411\u4E0A\u3055\u305B\u308B\u3053\
  \u3068\u304C\u76EE\u7684\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:42.242865-06:00'
model: gpt-4-0125-preview
summary: "PHP\u306B\u304A\u3051\u308BHTML\u306E\u30D1\u30FC\u30B9\u3068\u306F\u3001\
  HTML\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u304B\u3089\u7279\u5B9A\u306E\u60C5\u5831\
  \u3092\u62BD\u51FA\u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u3053\u306E\u4F5C\u696D\u3092\u884C\u3046\u306E\
  \u306F\u3001\u30C7\u30FC\u30BF\u62BD\u51FA\u306E\u81EA\u52D5\u5316\u3001\u30A6\u30A7\
  \u30D6\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3001\u307E\u305F\u306F\u69D8\u3005\
  \u306A\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\
  \u5F7C\u3089\u306E\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u3067\u7D71\
  \u5408\u3059\u308B\u305F\u3081\u3067\u3042\u308A\u3001\u624B\u52D5\u3067\u306E\u4ECB\
  \u5165\u306A\u3057\u306B\u6A5F\u80FD\u6027\u3092\u5411\u4E0A\u3055\u305B\u308B\u3053\
  \u3068\u304C\u76EE\u7684\u3067\u3059\u3002."
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

## 何となぜ？
PHPにおけるHTMLのパースとは、HTMLドキュメントから特定の情報を抽出することを指します。プログラマーがこの作業を行うのは、データ抽出の自動化、ウェブスクレイピング、または様々なウェブページのコンテンツを彼らのアプリケーション内で統合するためであり、手動での介入なしに機能性を向上させることが目的です。

## どのように：
PHPプログラマーは、ビルトイン関数を利用するか、Simple HTML DOM Parserのような堅牢なライブラリに頼ることができます。ここでは、PHPの`DOMDocument`とSimple HTML DOM Parserを使用した例を探ります。

### `DOMDocument`を使用する:
PHPの`DOMDocument`クラスは、DOM拡張の一部であり、HTMLやXMLドキュメントの解析や操作を可能にします。次は、`DOMDocument`を使用してHTMLドキュメント内のすべての画像を見つける方法の簡単な例です：

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>サンプルページ</title>
</head>
<body>
    <img src="image1.jpg" alt="イメージ 1">
    <img src="image2.jpg" alt="イメージ 2">
</body>
</html>
HTML;

$doc = new DOMDocument();
@$doc->loadHTML($html);
$images = $doc->getElementsByTagName('img');

foreach ($images as $img) {
    echo $img->getAttribute('src') . "\n";
}
```

サンプル出力：
```
image1.jpg
image2.jpg
```

### Simple HTML DOM Parserを使用する:
より複雑なタスクや簡単な構文が好みの場合、サードパーティのライブラリを使用することを好むかもしれません。Simple HTML DOM Parserは人気の選択肢であり、HTML構造をナビゲートおよび操作するためのjQueryのようなインターフェースを提供します。これを使う方法は以下の通りです：

まず、Composerを使用してライブラリをインストールします：
```
composer require simple-html-dom/simple-html-dom
```

次に、例えばすべてのリンクを見つけるためにHTMLを操作します：

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$client = new HtmlWeb();
$html = $client->load('http://www.example.com');

foreach($html->find('a') as $element) {
    echo $element->href . "\n";
}
```

このコードスニペットは、'http://www.example.com'のHTMLコンテンツを取得し、解析し、すべてのハイパーリンクを出力します。パースしたい実際のURLに`'http://www.example.com'`を置き換えてください。

これらの方法を利用して、PHP開発者は効果的にHTMLコンテンツを解析し、データ抽出を自分たちのニーズに合わせて調整したり、外部のウェブコンテンツをプロジェクトにシームレスに統合することができます。
