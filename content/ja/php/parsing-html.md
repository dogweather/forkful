---
title:                "HTMLの解析"
html_title:           "Arduino: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/parsing-html.md"
---

{{< edit_this_page >}}

## 何？そしてなぜ？
HTMLのパースはウェブページの内容を理解し操作する手段です。プログラマはこれを行うことで、HTMLの各要素に動的にアクセスし、データの取得、編集、または変換を行います。

## どうやって：
PHPにはDOM（Document Object Model）とSimpleXMLという便利な組み込みツールがあります。これらを使うとHTMLをパースすることができます。例を見てみましょう:

```PHP 
<?php
$dom = new DOMDocument();
@$dom->loadHTML('<p>Hello parse!</p>'); // Suppress warnings with @
$paragraphs = $dom->getElementsByTagName('p');
echo $paragraphs->item(0)->nodeValue; // Displays: Hello parse!
?>
```
このコードは段落 (`<p>`) タグを抽出し、そのテキストを表示します。

## 深掘り：
HTMLパーサーの歴史は長く、その発展はウェブテクノロジーの進化と共に歩んできました。それぞれが異なる方法を採用していますが、最終的な目標はすべて同じです：HTML文書を適切に解析してその構造を明らかにすること。

PHPでは、DOMやSimpleXML以外にも「phpQuery」「QueryPath」など、他のライブラリやツールも利用できます。これらはさらに詳細な操作が可能で、多機能なHTMLパーサーとして利用可能です。

実装の詳細については、たとえばHTML5に準拠したパーサーでは、HTML5の仕様を厳密に順守して、例外を見つけたり既知のバグを回避するための多くの特別な処理が含まれます。

## 参考リンク：
- PHP: DOMドキュメント - Manual
  [https://www.php.net/manual/en/class.domdocument.php](https://www.php.net/manual/en/class.domdocument.php)
- PHP: SimpleXML - Manual
  [https://www.php.net/manual/en/book.simplexml.php](https://www.php.net/manual/en/book.simplexml.php)
- phpQuery: A PHP Port of jQuery's Syntax
  [https://code.google.com/archive/p/phpquery/](https://code.google.com/archive/p/phpquery/)
- QueryPath: HTML/XML Manipulation, Traversal, and Extraction
  [https://querypath.org/](https://querypath.org/)