---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:58.954335-07:00
description: "T\u1EA3i m\u1ED9t trang web ngh\u0129a l\xE0 l\u1EA5y n\u1ED9i dung\
  \ c\u1EE7a trang \u0111\xF3, nh\u01B0 HTML, CSS v\xE0 JavaScript, m\u1ED9t c\xE1\
  ch l\u1EADp tr\xECnh. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m vi\u1EC7c n\xE0y \u0111\
  \u1EC3 x\u1EED l\xFD d\u1EEF li\u1EC7u,\u2026"
lastmod: 2024-02-19 22:04:55.650337
model: gpt-4-0125-preview
summary: "T\u1EA3i m\u1ED9t trang web ngh\u0129a l\xE0 l\u1EA5y n\u1ED9i dung c\u1EE7\
  a trang \u0111\xF3, nh\u01B0 HTML, CSS v\xE0 JavaScript, m\u1ED9t c\xE1ch l\u1EAD\
  p tr\xECnh. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m vi\u1EC7c n\xE0y \u0111\u1EC3\
  \ x\u1EED l\xFD d\u1EEF li\u1EC7u,\u2026"
title: "T\u1EA3i trang web"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tải một trang web nghĩa là lấy nội dung của trang đó, như HTML, CSS và JavaScript, một cách lập trình. Các lập trình viên làm việc này để xử lý dữ liệu, theo dõi sự thay đổi, hoặc kiểm tra ứng dụng web của họ.

## Cách làm:

```java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;

public class WebPageDownloader {
    public static void main(String[] args) {
        String urlStr = "http://example.com";
        try {
            URL url = new URL(urlStr);
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()))) {
                String dòng;
                while ((dòng = reader.readLine()) != null) {
                    System.out.println(dòng);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Đầu ra mẫu có thể nhìn như này:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
...
</html>
```

## Sâu hơn

Ngày xưa, việc tải một trang web là cơ bản—HTTP đơn giản, trang web chủ yếu là HTML tĩnh. Web hiện nay phức tạp—nghĩ về HTTPS, nội dung dựa trên JavaScript, và AJAX cả mớ.

Đối với nội dung tĩnh, `java.net.URL` và `java.net.HttpURLConnection` là lựa chọn đơn giản—không rườm rà, chỉ việc làm. Nhưng nếu bạn nhắm tới các trang đầy nội dung động được tải bởi JavaScript, những class này không đủ, và bạn phải nhìn vào các công cụ như Selenium hoặc HtmlUnit thay thế.

Đừng quên, việc chọn công cụ phù hợp cũng dựa vào bạn muốn làm gì với trang web sau khi đã tải. Phân tích HTML? Jsoup là bạn của bạn. Thực thi JavaScript? Cân nhắc một trình duyệt không giao diện. Các class của `java.net` chỉ là bước khởi đầu, nhưng chúng phù hợp cho các nhiệm vụ nhanh chóng hoặc lấy dữ liệu từ những trang web đơn thuần.

Nhớ về chính sách lịch sự: đừng tấn công một trang web bằng những yêu cầu liên tục, nếu không bạn sẽ bị cấm. Và chắc chắn rằng bạn đang tuân thủ hướng dẫn `robots.txt` của trang web.

## Xem thêm

- Thư viện [Jsoup](https://jsoup.org/) cho việc phân tích và trích xuất HTML.
- [Selenium WebDriver](https://www.selenium.dev/documentation/en/webdriver/) cho những nhiệm vụ phức tạp hơn bao gồm thực thi JavaScript.
- Hướng dẫn về [HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html) cho những ai muốn biết chi tiết về cách xử lý HTTP tích hợp trong Java.
- [HtmlUnit](http://htmlunit.sourceforge.net/), một "Trình duyệt không giao diện cho các chương trình Java", tuyệt vời cho các trang nặng về JavaScript.
