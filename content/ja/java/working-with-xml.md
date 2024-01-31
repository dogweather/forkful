---
title:                "XMLの扱い方"
date:                  2024-01-26T04:32:48.089392-07:00
model:                 gpt-4-0125-preview
simple_title:         "XMLの扱い方"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/working-with-xml.md"
---

{{< edit_this_page >}}

## 何となぜ？
JavaでのXMLの取り扱いには、XMLドキュメントのパース(解析)、クエリ、および操作が含まれます。プログラマーは、データ交換、設定管理、および多くのレガシーシステムやAPIがXMLを使用して通信するためにこれを行います。

## 方法:
Javaには、XMLを操作するためにDOM（Document Object Model）、SAX（Simple API for XML）、およびStAX（Streaming API for XML）のようなAPIが提供されています。次は、XMLファイルをパースするためのDOMの例です:

```java
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class XmlParser {
    public static void main(String[] args) {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse("data.xml");
            
            doc.getDocumentElement().normalize();
            NodeList nodeList = doc.getElementsByTagName("employee");
            
            for (int i = 0; i < nodeList.getLength(); i++) {
                Element element = (Element) nodeList.item(i);
                System.out.println("Name: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Age: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

`data.xml`が次のようであるとします:

```xml
<employees>
    <employee>
        <name>Jane Doe</name>
        <age>30</age>
    </employee>
    <employee>
        <name>John Doe</name>
        <age>40</age>
    </employee>
</employees>
```

出力は次のようになります:

```
Name: Jane Doe
Age: 30
Name: John Doe
Age: 40
```

## 深掘り
XMLは90年代後半から存在しており、異なるシステム間でデータを交換するための構造化された柔軟な方法を提供しています。JSONはそのシンプルな構文とJavaScriptとの密接な統合により新しいWeb APIに対してより人気がありますが、XMLはエンタープライズ環境、SOAPベースのWebサービス、およびMicrosoft Office用のOffice Open XMLのようなドキュメント標準で広く使用され続けています。

JavaでのXMLの解析に関しては、DOM APIは小さなドキュメントには適しています：これはツリーベースであり、メモリ内のXML構造に完全にアクセスできます。しかし、大きなファイルについては、メモリを多く消費する可能性があります。SAXとStAXはイベント駆動およびストリームベースであるため、よりメモリに優しいですが、XML構造のナビゲーションには便利ではないかもしれません。

XMLの作成または変更については、Javaはjavax.xml.transformおよびjavax.xml.bind（JAXB）パッケージも提供しています。JAXBはJava SEのバージョン10までの一部でしたが、その後Java SEからJava EEモジュールが削除されたため、別のライブラリになりました。これはJavaオブジェクトをXMLにシリアライズし、その逆を行う方法を提供するアノテーション駆動の方法です。

## 関連情報
JavaでのXMLの取り扱いに関するこれらの関連情報も参照してください:
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Java Architecture for XML Binding (JAXB)](https://javaee.github.io/jaxb-v2/)
- [OracleのJavaにおけるXMLガイド](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [W3C XMLテクノロジー](https://www.w3.org/standards/xml/)
- [Stack Overflow: 'java'および'xml'でタグ付けされた質問](https://stackoverflow.com/questions/tagged/java+xml)
