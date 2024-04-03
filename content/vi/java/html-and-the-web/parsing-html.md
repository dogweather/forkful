---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:03.942357-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y s\u1EED d\u1EE5ng Jsoup, m\u1ED9t th\u01B0\
  \ vi\u1EC7n ti\u1EC7n \xEDch khi l\xE0m vi\u1EC7c v\u1EDBi HTML th\u1EBF gi\u1EDB\
  i th\u1EF1c. \u0110\u1EA7u ti\xEAn, th\xEAm ph\u1EE5 thu\u1ED9c."
lastmod: '2024-03-13T22:44:36.486443-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y s\u1EED d\u1EE5ng Jsoup, m\u1ED9t th\u01B0 vi\u1EC7n ti\u1EC7n \xED\
  ch khi l\xE0m vi\u1EC7c v\u1EDBi HTML th\u1EBF gi\u1EDBi th\u1EF1c."
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
weight: 43
---

## Làm thế nào:
Hãy sử dụng Jsoup, một thư viện tiện ích khi làm việc với HTML thế giới thực. Đầu tiên, thêm phụ thuộc:

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.15.2</version>
</dependency>
```

Bây giờ đến phần thú vị. Dưới đây là cách để lấy tiêu đề của một trang web và in nó ra:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class HtmlParser {
    public static void main(String[] args) throws IOException {
        String url = "http://example.com";
        Document doc = Jsoup.connect(url).get();
        String title = doc.title();
        System.out.println("Tiêu đề: " + title);
    }
}
```

Kết quả:

```
Tiêu đề: Example Domain
```

Còn việc trích xuất tất cả các liên kết thì sao?

```java
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// ... bên trong method main hoặc một method khác
Elements links = doc.select("a[href]");
for (Element link : links) {
    System.out.println("Liên kết: " + link.attr("href"));
}
```

## Sâu hơn nữa
Ngày xưa, HTML được thuần hóa bởi các mẫu regex, một phương pháp dễ gặp lỗi và ác mộng đối với các tài liệu phức tạp. Enter Jsoup vào cuối những năm "aughts", cung cấp một giao diện giống jQuery cho Java để phân tích cú pháp, duyệt và thao tác HTML.

Jsoup không phải là lựa chọn duy nhất. Có HtmlUnit cho việc kiểm thử ứng dụng web toàn diện với hỗ trợ JavaScript, nhưng nó nặng hơn và phức tạp hơn. Đối với các tác vụ nhẹ nhàng hơn, Apache Commons Validator tuyệt vời chỉ để trích xuất URL.

Bên dưới cùng, Jsoup sử dụng một trình phân tích cú pháp DOM, mô hình hóa toàn bộ tài liệu trong bộ nhớ dưới dạng một cây. Cách tiếp cận này làm cho việc chọn và điều hướng cấu trúc HTML trở nên dễ dàng. Hơn nữa, nó linh hoạt với HTML lỗi, tự động sửa chữa các vấn đề để đảm bảo phân tích cú pháp mạnh mẽ.

Nhớ rằng, khi lấy dữ liệu, luôn kiểm tra `robots.txt` và điều khoản dịch vụ của trang web để tránh rắc rối pháp lý hoặc bị cấm IP.

## Xem thêm
- Tài liệu chính thức của Jsoup: https://jsoup.org/
- HtmlUnit: http://htmlunit.sourceforge.net/
- Apache Commons Validator: https://commons.apache.org/proper/commons-validator/
