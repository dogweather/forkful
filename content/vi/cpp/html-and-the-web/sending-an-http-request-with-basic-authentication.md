---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:10.615475-07:00
description: null
lastmod: '2024-03-13T22:44:37.042382-06:00'
model: gpt-4-0125-preview
summary: null
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

### Gì & Tại sao?
Gửi một yêu cầu HTTP với xác thực cơ bản bao gồm việc đính kèm tên người dùng và mật khẩu vào một yêu cầu để kiểm soát quyền truy cập. Lập trình viên làm điều này cho các kế hoạch xác thực đơn giản để bảo vệ tài nguyên trên máy chủ.

### Làm thế nào:
Đây là một ví dụ cơ bản sử dụng thư viện `CURL` trong C++. Trước khi bắt đầu, hãy chắc chắn rằng bạn đã cài đặt `libcurl`.

```C++
#include <iostream>
#include <curl/curl.h>

// Hàm callback đơn giản để xử lý dữ liệu nhận được bởi curl
static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://yourapi.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "pass");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        
        // Thực hiện yêu cầu, và kiểm tra lỗi
        res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() không thành công: %s\n", curl_easy_strerror(res));
        } else {
            std::cout << readBuffer << std::endl;
        }
        
        // Dọn dẹp
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Bạn sẽ thấy phản hồi từ máy chủ được in ra console, giả sử không có lỗi xảy ra.

### Sâu hơn nữa
Xác thực cơ bản là kiểu cũ, có từ những ngày đầu của HTTP. Giờ đây, ngành công nghiệp ưa chuộng các phương pháp an toàn hơn như OAuth và tokens. Mặc dù vậy, xác thực cơ bản vẫn được sử dụng, thường xuyên cho các hệ thống nội bộ hoặc đơn giản nơi mà các lớp bảo mật nặng nề là không cần thiết.

Bên dưới, tên người dùng và mật khẩu của bạn được mã hóa base64 và gói gọn trong tiêu đề HTTP. Nó đơn giản nhưng không an toàn nếu không qua HTTPS vì base64 dễ dàng đảo ngược - làm cho HTTPS trở thành một yêu cầu bắt buộc.

Nếu `libcurl` không phù hợp với bạn, hãy xem xét các lựa chọn khác như thư viện `cpp-httplib`, hoặc bạn có thể sử dụng `Boost.Beast` cho một phương pháp tiếp cận thực hành hơn.

### Xem thêm
- [libcurl](https://curl.se/libcurl/)
- [Kho lưu trữ GitHub cpp-httplib](https://github.com/yhirose/cpp-httplib)
- [Tài liệu Boost.Beast](https://www.boost.org/doc/libs/master/libs/beast/doc/html/index.html)
