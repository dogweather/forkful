---
title:                "Làm việc với XML"
aliases:
- /vi/java/working-with-xml/
date:                  2024-01-28T22:12:07.986917-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Làm việc với XML bao gồm việc phân tích cú pháp (parsing), truy vấn (querying), và thao tác (manipulating) các tài liệu XML bằng Java. Các lập trình viên thực hiện điều này để trao đổi dữ liệu, quản lý cấu hình và bởi vì nhiều hệ thống và API cũ sử dụng giao tiếp bằng XML.

## Làm thế nào:
Java cung cấp các API như DOM (Document Object Model), SAX (Simple API for XML), và StAX (Streaming API for XML) để làm việc với XML. Dưới đây là một ví dụ về DOM để phân tích một tệp XML:

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
                System.out.println("Tên: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Tuổi: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Giả sử `data.xml` trông như thế này:

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

Kết quả sẽ là:

```
Tên: Jane Doe
Tuổi: 30
Tên: John Doe
Tuổi: 40
```

## Thảo luận sâu hơn
XML đã tồn tại kể từ cuối những năm '90, cung cấp một cách có cấu trúc và linh hoạt để trao đổi dữ liệu giữa các hệ thống khác nhau. Mặc dù JSON đã trở nên phổ biến hơn cho các API web mới do cú pháp đơn giản hơn và tích hợp chặt chẽ với JavaScript, XML vẫn được sử dụng rộng rãi trong môi trường doanh nghiệp, các dịch vụ web dựa trên SOAP và các chuẩn tài liệu như Office Open XML cho Microsoft Office.

Khi nói đến việc phân tích XML trong Java, API DOM tuyệt vời cho các tài liệu nhỏ: nó dựa trên cây và cho phép truy cập đầy đủ vào cấu trúc XML trong bộ nhớ. Tuy nhiên, đối với các tệp lớn hơn, nó có thể gây ra tình trạng tiêu thụ nhiều bộ nhớ. SAX và StAX thân thiện hơn với bộ nhớ, vì chúng dựa trên sự kiện và luồng tương ứng, nhưng chúng có thể kém tiện lợi hơn khi điều hướng các cấu trúc XML.

Đối với việc tạo hoặc sửa đổi XML, Java cũng cung cấp các gói javax.xml.transform và javax.xml.bind (JAXB). JAXB là một phần của Java SE cho đến phiên bản 10, sau đó, nó trở thành một thư viện riêng rẽ do việc loại bỏ các module Java EE khỏi Java SE. Đây là cách dựa trên ghi chú để tuần tự hóa đối tượng Java thành XML và ngược lại.

## Xem thêm
Kiểm tra những nguồn liên quan khác để biết thêm về cách làm việc với XML trong Java:
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Java Architecture for XML Binding (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Hướng dẫn về XML trong Java của Oracle](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [Công nghệ XML của W3C](https://www.w3.org/standards/xml/)
- [Stack Overflow: Các câu hỏi được gắn thẻ 'java' và 'xml'](https://stackoverflow.com/questions/tagged/java+xml)
