---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:46.554410-07:00
description: "\u65B9\u6CD5\uFF1A HTML\u3092\u30D1\u30FC\u30B9\u3059\u308B\u305F\u3081\
  \u306BBash\u304C\u6700\u521D\u306B\u601D\u3044\u6D6E\u304B\u3076\u3082\u306E\u3067\
  \u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001`grep`\u3001`awk`\u3001`sed`\u3084\
  \u5916\u90E8\u30E6\u30FC\u30C6\u30A3\u30EA\u30C6\u30A3`lynx`\u306E\u3088\u3046\u306A\
  \u30C4\u30FC\u30EB\u3092\u5229\u7528\u3057\u3066\u884C\u3046\u3053\u3068\u304C\u3067\
  \u304D\u307E\u3059\u3002\u30ED\u30D0\u30B9\u30C8\u3055\u3092\u6C42\u3081\u308B\u306A\
  \u3089\u3070\u3001`libxml2`\u30D1\u30C3\u30B1\u30FC\u30B8\u304B\u3089`xmllint`\u3092\
  \u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.367034-06:00'
model: gpt-4-0125-preview
summary: "HTML\u3092\u30D1\u30FC\u30B9\u3059\u308B\u305F\u3081\u306BBash\u304C\u6700\
  \u521D\u306B\u601D\u3044\u6D6E\u304B\u3076\u3082\u306E\u3067\u306F\u3042\u308A\u307E\
  \u305B\u3093\u304C\u3001`grep`\u3001`awk`\u3001`sed`\u3084\u5916\u90E8\u30E6\u30FC\
  \u30C6\u30A3\u30EA\u30C6\u30A3`lynx`\u306E\u3088\u3046\u306A\u30C4\u30FC\u30EB\u3092\
  \u5229\u7528\u3057\u3066\u884C\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\
  \u30ED\u30D0\u30B9\u30C8\u3055\u3092\u6C42\u3081\u308B\u306A\u3089\u3070\u3001`libxml2`\u30D1\
  \u30C3\u30B1\u30FC\u30B8\u304B\u3089`xmllint`\u3092\u4F7F\u7528\u3057\u307E\u3059\
  ."
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

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
