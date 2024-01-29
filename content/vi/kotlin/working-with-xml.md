---
title:                "Làm việc với XML"
date:                  2024-01-28T22:11:44.756213-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với XML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?
Làm việc với XML bao gồm việc phân tích cú pháp, tạo và thao tác với các tài liệu XML - một ngôn ngữ đánh dấu dùng cho việc lưu trữ và truyền dữ liệu. Các lập trình viên thực hiện việc này bởi vì nhiều hệ thống vẫn trao đổi dữ liệu dưới dạng XML, và điều này cần thiết cho việc hỗ trợ cũ và tích hợp với các công nghệ hiện có.

## Làm Thế Nào:
Trong Kotlin, bạn có thể sử dụng `javax.xml.parsers` tích hợp sẵn để phân tích cú pháp:

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```

Để tạo các tài liệu XML, bạn có thể sử dụng `javax.xml.transform`:

```Kotlin
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import org.w3c.dom.Document
import java.io.StringWriter

fun convertDocumentToString(doc: Document): String {
    val transformer = TransformerFactory.newInstance().newTransformer()
    val result = StringWriter()
    transformer.transform(DOMSource(doc), StreamResult(result))
    return result.toString()
}
```

Kết quả mẫu cho việc chuyển đổi tài liệu thành Chuỗi sẽ đơn giản là nội dung XML của bạn dưới dạng định dạng chuỗi.

## Sâu Hơn
XML đã là một trụ cột của phát triển web và phần mềm kể từ những năm 90, được ưa chuộng vì tính dễ đọc và cấu trúc phân cấp của nó. Mặc dù JSON đã trở nên phổ biến đối với các dịch vụ web do đơn giản và kích thước thông điệp nhỏ hơn, XML vẫn thịnh hành trong môi trường doanh nghiệp, dịch vụ web dựa trên SOAP và cấu hình (như các tệp bố cục Android).

Có nhiều thư viện và API khác ngoài các tính năng tích hợp sẵn của Kotlin/Java cho việc xử lý XML, như Simple XML Serialization và Jackson XML module. Nhưng `javax.xml.parsers` và `javax.xml.transform` thường đáp ứng hầu hết nhu cầu mà không cần thêm phụ thuộc bên ngoài.

Khi xử lý XML trong Kotlin, các chi tiết thực hiện quan trọng bao gồm việc xử lý mã hóa ký tự đúng cách và quản lý thực thể XML để ngăn chặn các cuộc tấn công XML injection. Hãy chú ý đến các phức tạp về không gian tên và xác thực lược đồ khi phân tích cú pháp XML để đảm bảo tính toàn vẹn của dữ liệu.

## Xem Thêm
- [Tài Liệu Kotlin](https://kotlinlang.org/docs/reference/)
- [Tài Liệu DOM của Java](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML Serialization](http://simple.sourceforge.net/)
- [Mô-đun XML của Jackson](https://github.com/FasterXML/jackson-dataformat-xml)
