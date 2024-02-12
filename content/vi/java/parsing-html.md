---
title:                "Phân Tích Cú Pháp HTML"
aliases:
- vi/java/parsing-html.md
date:                  2024-01-28T22:04:03.942357-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân Tích Cú Pháp HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
