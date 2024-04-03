---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:30.530084-07:00
description: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP \u0111\u1EC3 l\u1EA5y d\u1EEF\
  \ li\u1EC7u t\u1EEB m\xE1y ch\u1EE7 web. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 t\u01B0\u01A1ng t\xE1c v\u1EDBi c\xE1c d\u1ECBch v\u1EE5\
  \ web, thu th\u1EADp th\xF4ng tin, ho\u1EB7c giao ti\u1EBFp gi\u1EEFa\u2026"
lastmod: '2024-03-13T22:44:37.038926-06:00'
model: gpt-4-0125-preview
summary: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP \u0111\u1EC3 l\u1EA5y d\u1EEF li\u1EC7\
  u t\u1EEB m\xE1y ch\u1EE7 web."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Cách thực hiện:
```c++
#include <iostream>
#include <cpr/cpr.h> // Đảm bảo đã cài đặt thư viện CPR trước

int main() {
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/get"});
    std::cout << r.text << std::endl; // Xuất phản hồi
    return 0;
}
```

Dữ liệu xuất ra mẫu:
```json
{
  "args": {},
  "headers": {
    "Accept": "*/*",
    "Host": "httpbin.org",
    "User-Agent": "curl/7.64.1"
  },
  "origin": "0.0.0.0",
  "url": "https://httpbin.org/get"
}
```

## Sâu hơn
Yêu cầu HTTP đã trở nên quan trọng kể từ khi web ra đời; chúng tuân theo mô hình client-server. Trước khi có thư viện C++ như CPR, việc gửi yêu cầu HTTP thường có nghĩa là sử dụng trực tiếp `libcurl`, hoặc tích hợp với ngôn ngữ khác tốt hơn cho giao tiếp web.

CPR, viết tắt của C++ Requests, là một bộ wrapper dễ sử dụng được truyền cảm hứng từ mô-đun `requests` của Python. Nó trừu tượng hóa nhiều phức tạp của `libcurl`. Vẫn còn tồn tại các lựa chọn khác, như Boost.Beast cho thao tác HTTP/S cấp thấp hơn, hoặc thư viện POCO cung cấp khả năng di động.

Khi đi sâu vào bên trong, việc gửi một yêu cầu HTTP bao gồm việc thiết lập một kết nối TCP, định dạng một yêu cầu tuân thủ với giao thức HTTP, sau đó phân tích phản hồi. Làm điều này đúng ngay từ đầu không phải là một việc đơn giản do xử lý lỗi, phức tạp của phiên bản HTTP, và các xem xét về an ninh.

## Tham khảo thêm
- Kho lưu trữ GitHub của CPR: https://github.com/libcpr/cpr
- tài liệu `libcurl` cho việc sử dụng nâng cao hơn: https://curl.se/libcurl/
- Tài liệu chính thức của Boost.Beast: https://www.boost.org/doc/libs/release/libs/beast/
- Tài liệu của POCO C++ Libraries: https://pocoproject.org/docs/
