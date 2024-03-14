---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:03.942357-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML ngh\u0129a l\xE0 \u0111\xE0o s\xE2\
  u v\xE0o \u0111\xE1nh d\u1EA5u \u0111\u1EC3 tr\xEDch xu\u1EA5t d\u1EEF li\u1EC7\
  u nh\u01B0 v\u0103n b\u1EA3n, li\xEAn k\u1EBFt ho\u1EB7c c\xE1c ph\u1EA7n t\u1EED\
  \ kh\xE1c. Ch\xFAng ta l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u01B0\u01A1ng\
  \ t\xE1c\u2026"
lastmod: '2024-03-13T22:44:36.486443-06:00'
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML ngh\u0129a l\xE0 \u0111\xE0o s\xE2u v\xE0\
  o \u0111\xE1nh d\u1EA5u \u0111\u1EC3 tr\xEDch xu\u1EA5t d\u1EEF li\u1EC7u nh\u01B0\
  \ v\u0103n b\u1EA3n, li\xEAn k\u1EBFt ho\u1EB7c c\xE1c ph\u1EA7n t\u1EED kh\xE1\
  c. Ch\xFAng ta l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u01B0\u01A1ng t\xE1c\u2026"
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Phân tích cú pháp HTML nghĩa là đào sâu vào đánh dấu để trích xuất dữ liệu như văn bản, liên kết hoặc các phần tử khác. Chúng ta làm điều này để tương tác với hoặc lấy dữ liệu từ nội dung web, tự động hóa các tác vụ duyệt web, hoặc kiểm thử ứng dụng web.

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
