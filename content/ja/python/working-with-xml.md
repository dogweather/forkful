---
title:                "XMLの扱い方"
date:                  2024-01-26T04:35:00.424244-07:00
model:                 gpt-4-0125-preview
simple_title:         "XMLの扱い方"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/working-with-xml.md"
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
