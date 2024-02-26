---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:46.554410-07:00
description: "HTML\u306E\u30D1\u30FC\u30B9\u3068\u306F\u3001HTML\u30D5\u30A1\u30A4\
  \u30EB\u306E\u69CB\u9020\u3068\u5185\u5BB9\u3092\u7CBE\u67FB\u3057\u3066\u60C5\u5831\
  \u3092\u62BD\u51FA\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u306B\u30A2\u30AF\
  \u30BB\u30B9\u3057\u305F\u308A\u3001\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\u64CD\u4F5C\
  \u3057\u305F\u308A\u3001\u30A6\u30A7\u30D6\u30B5\u30A4\u30C8\u304B\u3089\u60C5\u5831\
  \u3092\u62BD\u51FA\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.348445-07:00'
model: gpt-4-0125-preview
summary: "HTML\u306E\u30D1\u30FC\u30B9\u3068\u306F\u3001HTML\u30D5\u30A1\u30A4\u30EB\
  \u306E\u69CB\u9020\u3068\u5185\u5BB9\u3092\u7CBE\u67FB\u3057\u3066\u60C5\u5831\u3092\
  \u62BD\u51FA\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u306B\u30A2\u30AF\u30BB\
  \u30B9\u3057\u305F\u308A\u3001\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\u64CD\u4F5C\u3057\
  \u305F\u308A\u3001\u30A6\u30A7\u30D6\u30B5\u30A4\u30C8\u304B\u3089\u60C5\u5831\u3092\
  \u62BD\u51FA\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
title: "HTML\u306E\u89E3\u6790"
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
