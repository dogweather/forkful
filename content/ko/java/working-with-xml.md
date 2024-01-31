---
title:                "XML 다루기"
date:                  2024-01-26T04:32:47.572163-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML 다루기"

category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/working-with-xml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
Java를 사용하여 XML 작업은 파싱, 쿼리, 및 XML 문서의 조작을 포함합니다. 프로그래머는 데이터 교환, 설정 관리, 그리고 많은 레거시 시스템 및 API가 XML을 사용하여 통신하기 때문에 이 작업을 합니다.

## 방법:
Java는 DOM(Document Object Model), SAX(Simple API for XML), StAX(Streaming API for XML)와 같은 API를 제공하여 XML과 작업할 수 있습니다. 다음은 XML 파일을 파싱하는 DOM 예시입니다:

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

`data.xml` 이렇게 생겼다고 가정해 봅시다:

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

출력 결과는 다음과 같습니다:

```
Name: Jane Doe
Age: 30
Name: John Doe
Age: 40
```

## 심층 분석
XML은 90년대 후반부터 사용되어 왔으며, 다양한 시스템 간 데이터 교환을 위한 구조화되고 유연한 방법을 제공합니다. JSON이 그보다 더 단순한 문법과 JavaScript와의 긴밀한 통합으로 새로운 웹 API에 더 인기가 있지만, XML은 여전히 엔터프라이즈 환경, SOAP 기반 웹 서비스 및 Microsoft Office의 Office Open XML과 같은 문서 표준에서 널리 사용됩니다.

Java에서 XML을 파싱할 때, DOM API는 작은 문서에 적합합니다: 이는 트리 기반으로 메모리 내 XML 구조에 대한 전체 접근을 가능하게 합니다. 하지만, 큰 파일을 다룰 때는 메모리를 많이 사용할 수 있습니다. SAX와 StAX는 각각 이벤트 기반 및 스트림 기반이므로 더 메모리 친화적이지만, XML 구조를 탐색하는 데에는 덜 편리할 수 있습니다.

XML을 생성하거나 수정하기 위해, Java는 javax.xml.transform 및 javax.xml.bind(JAXB) 패키지를 제공합니다. JAXB는 Java SE 버전 10까지 Java SE의 일부였으나, Java EE 모듈의 Java SE에서 제거됨에 따라 별도의 라이브러리가 되었습니다. 이는 Java 객체를 XML로 직렬화하고 그 반대로 하는 주석 기반 방식입니다.

## 관련 자료
Java에서 XML 작업에 대한 더 많은 정보를 위한 관련 자료들을 확인해 보세요:
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Java Architecture for XML Binding (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Oracle의 Java에서 XML 가이드](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [W3C XML 기술](https://www.w3.org/standards/xml/)
- [Stack Overflow: 'java' 및 'xml' 태그가 붙은 질문들](https://stackoverflow.com/questions/tagged/java+xml)
