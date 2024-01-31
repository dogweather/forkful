---
title:                "XMLの扱い方"
date:                  2024-01-26T04:28:13.421436-07:00
model:                 gpt-4-0125-preview
simple_title:         "XMLの扱い方"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/working-with-xml.md"
---

{{< edit_this_page >}}

## 何となぜ?
C言語でのXMLの取り扱いには、解析、作成、およびXMLファイルの操作が含まれています - 本質的には構造化されたデータストレージです。プログラマーはこれを行うことで、設定、データ交換などに使用される、ポータブルで人間が読める形式のデータと対話します。

## 方法:
以下は、XMLファイルを解析し、ルート要素を取得するための`libxml2`ライブラリを使用するスニペットです。

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    // XMLファイルをパースする
    doc = xmlReadFile("example.xml", NULL, 0);

    // ルート要素を取得する
    root_element = xmlDocGetRootElement(doc);

    printf("Root Element: %s\n", root_element->name);

    // ドキュメントを解放する
    xmlFreeDoc(doc);

    // パーサーのクリーンアップ
    xmlCleanupParser();

    return 0;
}
```

ルートが`<data>`のXMLの場合のサンプル出力：
```
Root Element: data
```

## 深堀り
XML、またはExtensible Markup Languageは、90年代後半にデータを記述および構造化する方法を提供するために登場しました。C言語では、`libxml2`が主に使用されています。それは堅牢ですが、XML初心者にとって最も簡単ではありません。代替品には、より軽量で初心者に優しい`tinyxml2`があります。実装に関しては、Cには組み込みのXMLサポートがないため、ライブラリがそのギャップを埋めます。これらはサイズ、速度、複雑さ、および移植性で異なります。ほとんどはDOMおよびSAX解析方法を提供しています：DOMはメモリに全体をロードし、小規模なドキュメントに適しています；SAXはイベント駆動型で、要素をその場で扱うため、大きなファイルに適しています。どちらも使用例とトレードオフがあります。

## 参照
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 on GitHub](https://github.com/leethomason/tinyxml2)
- [w3schoolsによるXMLチュートリアル](https://www.w3schools.com/xml/)
- [W3CによるXML仕様](https://www.w3.org/XML/)
