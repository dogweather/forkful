---
date: 2024-01-26 04:32:48.089392-07:00
description: "\u65B9\u6CD5: Java\u306B\u306F\u3001XML\u3092\u64CD\u4F5C\u3059\u308B\
  \u305F\u3081\u306BDOM\uFF08Document Object Model\uFF09\u3001SAX\uFF08Simple API\
  \ for XML\uFF09\u3001\u304A\u3088\u3073StAX\uFF08Streaming API for XML\uFF09\u306E\
  \u3088\u3046\u306AAPI\u304C\u63D0\u4F9B\u3055\u308C\u3066\u3044\u307E\u3059\u3002\
  \u6B21\u306F\u3001XML\u30D5\u30A1\u30A4\u30EB\u3092\u30D1\u30FC\u30B9\u3059\u308B\
  \u305F\u3081\u306EDOM\u306E\u4F8B\u3067\u3059."
lastmod: '2024-03-13T22:44:41.981590-06:00'
model: gpt-4-0125-preview
summary: "Java\u306B\u306F\u3001XML\u3092\u64CD\u4F5C\u3059\u308B\u305F\u3081\u306B\
  DOM\uFF08Document Object Model\uFF09\u3001SAX\uFF08Simple API for XML\uFF09\u3001\
  \u304A\u3088\u3073StAX\uFF08Streaming API for XML\uFF09\u306E\u3088\u3046\u306A\
  API\u304C\u63D0\u4F9B\u3055\u308C\u3066\u3044\u307E\u3059\u3002\u6B21\u306F\u3001\
  XML\u30D5\u30A1\u30A4\u30EB\u3092\u30D1\u30FC\u30B9\u3059\u308B\u305F\u3081\u306E\
  DOM\u306E\u4F8B\u3067\u3059."
title: "XML\u306E\u6271\u3044\u65B9"
weight: 40
---

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
