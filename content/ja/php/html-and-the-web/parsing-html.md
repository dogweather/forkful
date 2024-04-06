---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:05.177864-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A PHP\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30D3\u30EB\u30C8\u30A4\u30F3\u95A2\u6570\u3092\u5229\u7528\u3059\
  \u308B\u304B\u3001Simple HTML DOM Parser\u306E\u3088\u3046\u306A\u5805\u7262\u306A\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u983C\u308B\u3053\u3068\u304C\u3067\u304D\u307E\
  \u3059\u3002\u3053\u3053\u3067\u306F\u3001PHP\u306E`DOMDocument`\u3068Simple HTML\
  \ DOM Parser\u3092\u4F7F\u7528\u3057\u305F\u4F8B\u3092\u63A2\u308A\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:43.097405-06:00'
model: gpt-4-0125-preview
summary: ''
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

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
