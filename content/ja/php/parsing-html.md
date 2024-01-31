---
title:                "HTMLの解析"
date:                  2024-01-20T15:32:48.675616-07:00
html_title:           "Arduino: HTMLの解析"
simple_title:         "HTMLの解析"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTMLパース（解析）は、HTML文書からデータを抽出や操作することです。プログラマーはこれを使って、Webスクレイピングやコンテンツの自動処理を行います。

## How to: (やり方)
PHPでは、DOMDocumentクラスを使ってHTMLを簡単にパースできます。以下は基本的な例です。

```php
<?php
$doc = new DOMDocument();
@$doc->loadHTML(file_get_contents('https://example.com'));
$tags = $doc->getElementsByTagName('a');

foreach ($tags as $tag) {
    echo $tag->getAttribute('href') . PHP_EOL;
}
?>
```

これはウェブページから全てのリンクを抜き出して表示します。

## Deep Dive (深掘り)
HTMLパースは、PHPがはじめから備えている機能じゃありません。初期のPHPでは正規表現や文字列関数でHTMLを扱っていましたが、これは脆弱でエラーが多い方法です。そのため、DOMDocumentクラスが導入され、XMLとHTMLを安全かつ正確に解析する手法が可能になりました。他の方法にはSimple HTML DOM Parserやthird-partyライブラリがありますが、DOMDocumentが最も一般的です。パフォーマンスやメモリ使用について理解することも重要です。大きなHTMLドキュメントでは、資源を大量に使用することがあります。

## See Also (関連項目)
- [PHP: DOMDocument - Manual](https://www.php.net/manual/en/class.domdocument.php)
- [PHP: DOMElement - Manual](https://www.php.net/manual/en/class.domelement.php)
- [PHP Simple HTML DOM Parser](http://simplehtmldom.sourceforge.net/)
