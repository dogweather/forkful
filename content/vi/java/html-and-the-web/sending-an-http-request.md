---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:00.973927-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y s\u1EED d\u1EE5ng `HttpClient`, `HttpRequest`,\
  \ v\xE0 `HttpResponse` c\u1EE7a Java 11 \u0111\u1EC3 th\u1EF1c hi\u1EC7n m\u1ED9\
  t y\xEAu c\u1EA7u GET v\xE0 l\u1EA5y m\u1ED9t s\u1ED1 d\u1EEF li\u1EC7u."
lastmod: '2024-03-13T22:44:36.485145-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y s\u1EED d\u1EE5ng `HttpClient`, `HttpRequest`, v\xE0 `HttpResponse`\
  \ c\u1EE7a Java 11 \u0111\u1EC3 th\u1EF1c hi\u1EC7n m\u1ED9t y\xEAu c\u1EA7u GET\
  \ v\xE0 l\u1EA5y m\u1ED9t s\u1ED1 d\u1EEF li\u1EC7u."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Làm thế nào:
Hãy sử dụng `HttpClient`, `HttpRequest`, và `HttpResponse` của Java 11 để thực hiện một yêu cầu GET và lấy một số dữ liệu:

```java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class HttpRequestExample {
    public static void main(String[] args) {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                              .uri(URI.create("http://example.com"))
                              .build();

        client.sendAsync(request, HttpResponse.BodyHandlers.ofString())
              .thenApply(HttpResponse::body)
              .thenAccept(System.out::println)
              .join();
    }
}
```

Bạn chạy nó, và voilà—phản hồi từ máy chủ, ngay trên console của bạn.

## Sâu hơn nữa
Trước Java 11, việc gửi một yêu cầu HTTP là một vũ điệu phức tạp hơn, thường yêu cầu sử dụng các thư viện của bên thứ ba như Apache HttpClient. `HttpURLConnection` cũng là một lựa chọn nhưng cảm thấy như một con khủng long—cồng kềnh và ít trực quan.

Với Java 11, `HttpClient` xuất hiện, đơn giản hóa quy trình với cả hai phương pháp đồng bộ `.send` và bất đồng bộ `.sendAsync`. Nó phản ứng và không chặn—nghĩa là bạn không phải ngồi chờ trong lúc nó thực hiện công việc của mình. Điều này phù hợp với nhu cầu hiệu quả của ứng dụng hiện đại, nơi mà việc chờ đợi là lãng phí thời gian.

Các lựa chọn thay thế cho các thư viện tiêu chuẩn của Java? Các thư viện như OkHttp và Retrofit vẫn là sự yêu thích khi các tính năng mạnh mẽ và cấu hình tùy chỉnh được mong muốn. Và tại sao không? Chúng đi kèm với những ưu điểm của riêng mình, như quản lý kết nối và chuyển đổi cuộc gọi ngay lập tức.

## Xem thêm
Đào sâu hơn vào HttpClient của Java với tài liệu chính thức của Java:
- [HttpClient](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [HttpRequest](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpRequest.html)
- [HttpResponse](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpResponse.html)

Cảm thấy thích phiêu lưu? Khám phá OkHttp và Retrofit:
- [OkHttp](https://square.github.io/okhttp/)
- [Retrofit](https://square.github.io/retrofit/)
