---
date: 2024-01-26 04:35:00.424244-07:00
description: "\u300CXML\u3092\u6271\u3046\u300D\u3068\u306F\u3001\u30D7\u30ED\u30B0\
  \u30E9\u30DF\u30F3\u30B0\u3092\u4F7F\u7528\u3057\u3066XML\uFF08eXtensible Markup\
  \ Language\uFF09\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u53D6\u3063\u305F\u308A\
  \u3001\u4F5C\u6210\u3057\u305F\u308A\u3001\u5909\u66F4\u3057\u305F\u308A\u3059\u308B\
  \u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u304C\u3053\u308C\u3092\u884C\u3046\u7406\u7531\u306F\u3001XML\u304C\u30D7\u30E9\
  \u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\u306B\u4F9D\u5B58\u3057\u306A\u3044\u6027\u8CEA\
  \u3068\u81EA\u5DF1\u8A18\u8FF0\u5F62\u5F0F\u3067\u30C7\u30FC\u30BF\u4EA4\u63DB\u306B\
  \u5E83\u304F\u5229\u7528\u3055\u308C\u3066\u3044\u308B\u304B\u3089\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:41.532726-06:00'
model: gpt-4-0125-preview
summary: "\u300CXML\u3092\u6271\u3046\u300D\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\
  \u30DF\u30F3\u30B0\u3092\u4F7F\u7528\u3057\u3066XML\uFF08eXtensible Markup Language\uFF09\
  \u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u53D6\u3063\u305F\u308A\u3001\u4F5C\u6210\
  \u3057\u305F\u308A\u3001\u5909\u66F4\u3057\u305F\u308A\u3059\u308B\u3053\u3068\u3092\
  \u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u3053\u308C\
  \u3092\u884C\u3046\u7406\u7531\u306F\u3001XML\u304C\u30D7\u30E9\u30C3\u30C8\u30D5\
  \u30A9\u30FC\u30E0\u306B\u4F9D\u5B58\u3057\u306A\u3044\u6027\u8CEA\u3068\u81EA\u5DF1\
  \u8A18\u8FF0\u5F62\u5F0F\u3067\u30C7\u30FC\u30BF\u4EA4\u63DB\u306B\u5E83\u304F\u5229\
  \u7528\u3055\u308C\u3066\u3044\u308B\u304B\u3089\u3067\u3059\u3002"
title: "XML\u306E\u6271\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？
「XMLを扱う」とは、プログラミングを使用してXML（eXtensible Markup Language）ファイルを読み取ったり、作成したり、変更したりすることを指します。プログラマーがこれを行う理由は、XMLがプラットフォームに依存しない性質と自己記述形式でデータ交換に広く利用されているからです。

## 方法：
Pythonの`xml.etree.ElementTree`モジュールは、XMLを扱うためのツールを提供しています。

XMLドキュメントを解析する：
```python
import xml.etree.ElementTree as ET

xml_data = """<?xml version="1.0"?>
<library>
    <book>
        <title>Learning Python</title>
        <author>Mark Lutz</author>
    </book>
    <book>
        <title>Programming Python</title>
        <author>Mark Lutz</author>
    </book>
</library>
"""

root = ET.fromstring(xml_data)
for book in root.findall('book'):
    title = book.find('title').text
    author = book.find('author').text
    print(f'タイトル: {title}, 著者: {author}')
```
サンプル出力：
```
タイトル: Learning Python, 著者: Mark Lutz
タイトル: Programming Python, 著者: Mark Lutz
```

XMLドキュメントを作成する：
```python
library = ET.Element('library')
book = ET.SubElement(library, 'book')
title = ET.SubElement(book, 'title')
title.text = 'Automate the Boring Stuff with Python'
author = ET.SubElement(book, 'author')
author.text = 'Al Sweigart'

tree = ET.ElementTree(library)
tree.write('library.xml')
```

## 深堀り：
XMLは、簡易なオンラインデータ共有のためにSGMLの単純化したサブセットとして90年代後半に作られました。ウェブデータにおけるJSONの人気が高まっているにもかかわらず、XMLは多くの企業、構成、およびウェブサービス（SOAP、RSS）にとって重要であり続けています。

`xml.etree.ElementTree`の代替品には、`lxml`や`minidom`があります。`lxml`はより高速で機能豊富な一方で、`minidom`はより「DOMライク」なXMLインターフェイスを提供します。選択する際には、使いやすさ、パフォーマンス、特定の機能要件を考慮してください。

内部では、`ElementTree`は要素ツリーモデル上で操作を行います。XMLファイルの各コンポーネントがツリー内のノードとなります。これにより、直感的なパス式や検索が可能となり、XMLデータの構造を容易にナビゲートおよび操作できます。

## 参照：
- Python `xml.etree.ElementTree`モジュール：https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`：https://lxml.de/
- W3Schools XMLチュートリアル：https://www.w3schools.com/xml/
- XML仕様：https://www.w3.org/XML/
