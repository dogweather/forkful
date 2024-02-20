---
date: 2024-01-26 04:32:48.089392-07:00
description: "Java\u3067\u306EXML\u306E\u53D6\u308A\u6271\u3044\u306B\u306F\u3001\
  XML\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u306E\u30D1\u30FC\u30B9(\u89E3\u6790)\u3001\
  \u30AF\u30A8\u30EA\u3001\u304A\u3088\u3073\u64CD\u4F5C\u304C\u542B\u307E\u308C\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u4EA4\
  \u63DB\u3001\u8A2D\u5B9A\u7BA1\u7406\u3001\u304A\u3088\u3073\u591A\u304F\u306E\u30EC\
  \u30AC\u30B7\u30FC\u30B7\u30B9\u30C6\u30E0\u3084API\u304CXML\u3092\u4F7F\u7528\u3057\
  \u3066\u901A\u4FE1\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\
  \u3059\u3002"
lastmod: 2024-02-19 22:05:01.140048
model: gpt-4-0125-preview
summary: "Java\u3067\u306EXML\u306E\u53D6\u308A\u6271\u3044\u306B\u306F\u3001XML\u30C9\
  \u30AD\u30E5\u30E1\u30F3\u30C8\u306E\u30D1\u30FC\u30B9(\u89E3\u6790)\u3001\u30AF\
  \u30A8\u30EA\u3001\u304A\u3088\u3073\u64CD\u4F5C\u304C\u542B\u307E\u308C\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u4EA4\u63DB\
  \u3001\u8A2D\u5B9A\u7BA1\u7406\u3001\u304A\u3088\u3073\u591A\u304F\u306E\u30EC\u30AC\
  \u30B7\u30FC\u30B7\u30B9\u30C6\u30E0\u3084API\u304CXML\u3092\u4F7F\u7528\u3057\u3066\
  \u901A\u4FE1\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
title: "XML\u306E\u6271\u3044\u65B9"
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
