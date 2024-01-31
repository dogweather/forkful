---
title:                "Gửi một yêu cầu HTTP với xác thực cơ bản"
date:                  2024-01-28T22:08:08.267031-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP với xác thực cơ bản"

category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Gửi một yêu cầu HTTP với xác thực cơ bản bao gồm việc thêm một header với tên người dùng và mật khẩu để truy cập vào các tài nguyên được bảo vệ. Các lập trình viên thực hiện điều này để tương tác với các dịch vụ web yêu cầu thông tin đăng nhập để hoạt động.

## Cách thực hiện:
Để gửi một yêu cầu HTTP với xác thực cơ bản trong C, bạn thường sử dụng thư viện như libcurl. Dưới đây là một ví dụ ngắn:

```c
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl = curl_easy_init();
    if(curl) {
        // Đặt URL sắp nhận yêu cầu POST của chúng ta
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        
        // Đặt thông tin xác thực cơ bản
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");
        
        // Thực hiện yêu cầu
        CURLcode res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() thất bại: %s\n", curl_easy_strerror(res));
        }
        
        // Dọn dẹp
        curl_easy_cleanup(curl);
    }
    
    return 0;
}
```

Đầu ra sẽ phụ thuộc vào phản hồi từ máy chủ.

## Sâu hơn
Gửi một yêu cầu HTTP với xác thực cơ bản là một cách khá cũ để kiểm soát quyền truy cập vào các tài nguyên web. Được thiết kế từ những ngày đầu của internet, đây không phải là phương pháp an toàn nhất vì thông tin xác thực được mã hóa bằng base64, không được mã hóa.

Các phương thức khác như OAuth và khóa API hiện được khuyến khích sử dụng để đảm bảo an ninh tốt hơn. Tuy nhiên, xác thực cơ bản vẫn hữu ích cho các script đơn giản hoặc công cụ nội bộ trong trường hợp những rủi ro này được chấp nhận.

Việc thực hiện thường được thực hiện với các thư viện như libcurl hoặc lập trình socket tùy chỉnh nếu bạn cần kiểm soát nhiều hơn. Các header xác thực cơ bản có thể được xây dựng thủ công, nhưng điều này khá phiền phức và dễ sai, vì vậy việc sử dụng thư viện là con đường tốt nhất.

## Xem thêm
- Tài liệu thư viện cURL: https://curl.haxx.se/libcurl/c/
- RFC 7617, The 'Basic' HTTP Authentication Scheme: https://tools.ietf.org/html/rfc7617
- Tài liệu về xác thực HTTP trên MDN web docs: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Giới thiệu về OAuth: https://oauth.net/
