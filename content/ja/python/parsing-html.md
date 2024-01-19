---
title:                "HTMLの解析"
html_title:           "Arduino: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/parsing-html.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTMLの解析は、HTMLのコードを解読してそれがどう組み立てられているかを理解することです。プログラマーはこれを、データを抽出したり、ウェブスクレイピングしたり、ウェブサイトをデバッグしたりするために行います。

## 手順：

Pythonでは、「BeautifulSoup」というライブラリを使ってHTMLを簡単に解析できます。例を見てみましょう。

```Python
from bs4 import BeautifulSoup

html_doc = """
<html><head><title>サイトタイトル</title></head>
<body>
<p class="title"><b>ストーリータイトル</b></p>
<a href="http://example.com/elsie" class="sister" id="link1">イルシー</a>,
<a href="http://example.com/lacie" class="sister" id="link2">レイシー</a>
</body>
</html>
"""

soup = BeautifulSoup(html_doc, 'html.parser')

print(soup.prettify())
```

上のコードを実行すると次のような結果が得られます。

```
<html>
 <head>
  <title>
   サイトタイトル
  </title>
 </head>
 <body>
  <p class="title">
   <b>
    ストーリータイトル
   </b>
  </p>
  <a class="sister" href="http://example.com/elsie" id="link1">
   イルシー
  </a>
  ,
  <a class="sister" href="http://example.com/lacie" id="link2">
   レイシー
  </a>
 </body>
</html>
```

## ディープダイブ：

HTML解析の歴史的な背景については、1990年代初頭に誕生したHTMLが、現在のウェブの基礎を築くインターネットの革命の一部だったと言えます。そこから、ウェブコンテンツの動的な解析と抽出の必要性が生まれ、今日のHTML解析の技術が開発されました。

BeautifulSoupのようなライブラリの代わりにJavaScriptや他のプログラミング言語を使ってHTMLを解析する方法もありますが、Pythonはこの目的に特に優れています。なぜなら、BeautifulSoupはあらゆる種類のコードを簡単に解析でき、ウェブスクレイピング作業を容易にします。

実装については、BeautifulSoupは様々なパーサー（'html.parser'、'lxml'、'html5lib'など）を用いてHTML文書を解析します。これらのパーサーは、HTMLの構造を解釈し、Pythonオブジェクトとして利用できるようにします。

## 参照リンク：

- BeautifulSoup公式ドキュメンテーション: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Pythonによるウェブスクレイピング: https://realpython.com/beautiful-soup-web-scraper-python/
- 'html.parser' vs 'lxml' の比較: https://lxml.de/tutorial.html