---
title:                "HTMLの解析"
aliases: - /ja/bash/parsing-html.md
date:                  2024-02-03T19:11:46.554410-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTMLの解析"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

HTMLのパースとは、HTMLファイルの構造と内容を精査して情報を抽出することを意味します。プログラマーは、データにアクセスしたり、コンテンツを操作したり、ウェブサイトから情報を抽出したりするためにこれを行います。

## 方法：

HTMLをパースするためにBashが最初に思い浮かぶものではありませんが、`grep`、`awk`、`sed`や外部ユーティリティ`lynx`のようなツールを利用して行うことができます。ロバストさを求めるならば、`libxml2`パッケージから`xmllint`を使用します。

```bash
# 必要であれば xmllint をインストール
sudo apt-get install libxml2-utils

# サンプル HTML
cat > sample.html <<EOF
<html>
<head>
  <title>Sample Page</title>
</head>
<body>
  <h1>Hello, Bash!</h1>
  <p id="myPara">Bash can read me.</p>
</body>
</html>
EOF

# タイトルをパースする
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "タイトルは： $title"

# IDによる段落の抽出
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "段落の内容は： $para"
```

出力：
```
タイトルは： Sample Page
段落の内容は： Bash can read me.
```

## 深堀り

昔、プログラマーは`grep`のような正規表現ベースのツールを使ってHTMLをスキャンしていましたが、それは不格好でした。HTMLは規則正しくなく、文脈に依存しています。伝統的なツールはこれを見落とし、エラーが発生しやすいです。

代替手段は？ 多数あります。Beautiful Soupを使ったPython、DOMDocumentを使ったPHP、DOMパーサーを使ったJavaScript—HTMLの構造を理解するように設計されたライブラリを持つ言語です。

bashスクリプトでの`xmllint`の使用は、単純な作業には確かです。XMLを理解し、その延長でXHTMLを理解します。ただし、通常のHTMLは予測不可能な場合があります。XMLの厳格なルールには常に従わないためです。`xmllint`はHTMLをXMLモデルに強制することで、形式が整っているHTMLにはうまく機能しますが、乱雑なものについては躓く可能性があります。

## 参照

- [W3Schools - HTML DOM Parser](https://www.w3schools.com/xml/dom_intro.asp): HTML DOMを解説。
- [MDN Web Docs - XMLのパースとシリアライズ](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): XHTMLに適用されるXMLパースの原則について。
- [Beautiful Soup Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): HTMLパースを行うPythonライブラリ。
- [libxml2 Documentation](http://xmlsoft.org/): `xmllint`および関連するXMLツールに関する詳細。
