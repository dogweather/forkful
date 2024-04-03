---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:24.306478-07:00
description: "L\xE0m th\u1EBF n\xE0o: Java l\xE0m cho vi\u1EC7c g\u1EEDi y\xEAu c\u1EA7\
  u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3n tr\u1EDF n\xEAn kh\xE1 \u0111\u01A1\
  n gi\u1EA3n s\u1EED d\u1EE5ng l\u1EDBp `HttpURLConnection`. \u0110\xE2y l\xE0 m\u1ED9\
  t v\xED d\u1EE5 nhanh."
lastmod: '2024-03-13T22:44:36.488941-06:00'
model: gpt-4-0125-preview
summary: "Java l\xE0m cho vi\u1EC7c g\u1EEDi y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c\
  \ th\u1EF1c c\u01A1 b\u1EA3n tr\u1EDF n\xEAn kh\xE1 \u0111\u01A1n gi\u1EA3n s\u1EED\
  \ d\u1EE5ng l\u1EDBp `HttpURLConnection`."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

## Làm thế nào:
Java làm cho việc gửi yêu cầu HTTP với xác thực cơ bản trở nên khá đơn giản sử dụng lớp `HttpURLConnection`. Đây là một ví dụ nhanh:

```java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

public class BasicAuthRequest {

    public static void main(String[] args) {
        try {
            URL url = new URL("http://example.com/resource");
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            
            String userCredentials = "user:password";
            String basicAuth = "Basic " + new String(Base64.getEncoder().encode(userCredentials.getBytes(StandardCharsets.UTF_8)));
            connection.setRequestProperty("Authorization", basicAuth);

            int responseCode = connection.getResponseCode();
            System.out.println("Mã Phản Hồi: " + responseCode);

            if (responseCode == HttpURLConnection.HTTP_OK) {
                BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                String inputLine;
                StringBuilder response = new StringBuilder();

                while ((inputLine = in.readLine()) != null) {
                    response.append(inputLine);
                }
                in.close();

                System.out.println(response.toString());
            } else {
                System.out.println("Yêu cầu GET không thành công");
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
Kết quả Mẫu:
```
Mã Phản Hồi: 200
{ "message": "Đây là phản hồi từ một tài nguyên được bảo vệ!" }
```

## Tìm hiểu sâu hơn
Xác thực cơ bản đã có từ những ngày đầu của HTTP. Nó hoạt động bằng cách truyền các thông tin đăng nhập mã hóa dạng base64 trong header, làm cho nó đơn giản nhưng không rất an toàn mà không có HTTPS, vì thông tin đăng nhập có thể dễ dàng được giải mã.

Những phương pháp thay thế như OAuth thêm một tầng bảo mật khác bằng cách sử dụng token. Xác thực dựa trên token hiện là lựa chọn được ưu tiên hiện nay, đặc biệt là cho các RESTful APIs.

Khi triển khai xác thực truy cập cơ bản trong Java, cách được khuyến nghị từ Java 11 là sử dụng lớp mới `HttpClient`. Nó linh hoạt hơn và hỗ trợ HTTP/2 ngay lập tức. Tuy nhiên, đối với các yêu cầu cơ bản hoặc hệ thống cũ, `HttpURLConnection` vẫn là một lựa chọn khả thi.

## Xem thêm
- [RFC 7617 - Sơ đồ Xác thực HTTP 'Cơ bản'](https://tools.ietf.org/html/rfc7617)
- [Tài liệu API Java 11 HTTP Client của Oracle](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [Hướng dẫn Baeldung về yêu cầu HTTP trong Java](https://www.baeldung.com/java-http-request)
