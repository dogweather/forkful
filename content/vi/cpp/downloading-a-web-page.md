---
title:                "Tải trang web"
aliases:
- vi/cpp/downloading-a-web-page.md
date:                  2024-01-28T21:59:56.270689-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tải trang web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tải xuống một trang web đơn giản chỉ có nghĩa là lấy nội dung của nó, thường là ở định dạng HTML, để xem hoặc xử lý cục bộ. Lập trình viên tải xuống các trang web để gỡ lỗi dữ liệu, theo dõi các thay đổi hoặc tích hợp với các dịch vụ web.

## Cách thức:
Trong phiên bản C++ hiện tại, bạn có thể sử dụng thư viện `CURL` để tải xuống nội dung web. Dưới đây là một ví dụ cơ bản:

```cpp
#include <curl/curl.h>
#include <iostream>
#include <string>

static size_t writeCallback(void* nội dung, size_t kích thước, size_t nmemb, void* userp){
    ((std::string*)userp)->append((char*)nội dung, kích thước * nmemb);
    return kích thước * nmemb;
}

int main() {
    CURL* curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);

        if(res == CURLE_OK) {
            std::cout << readBuffer << std::endl;
        }
        else {
            std::cerr << "Lỗi CURL: " << curl_easy_strerror(res) << std::endl;
        }
    }

    return 0;
}
```

Đầu ra mẫu:

```html
<!doctype html>
<html>
<head>
    <title>Ví dụ miền</title>
    ...
</head>
<body>
    <div>
        <h1>Ví dụ miền</h1>
        <p>Miền này được sử dụng cho các ví dụ minh họa trong tài liệu. Bạn có thể sử dụng miền này ...</p>
    </div>
</body>
</html>
```

## Sâu hơn
Ban đầu, không có cách chuẩn nào để tải các trang web chỉ với C++. Lập trình viên sử dụng các giải pháp cụ thể cho nền tảng hoặc các thư viện bên thứ ba khác nhau. Hiện nay, `libcurl` là một thư viện được hỗ trợ rộng rãi và đa dạng cho việc truyền dữ liệu với các URL. Được biên dịch và liên kết với mã C++ của bạn, curl là công cụ đi đến.

Các giải pháp thay thế cho libcurl bao gồm HTTPClientSession của Poco và C++ Rest SDK (còn được gọi là Casablanca). Trong khi libcurl là dựa trên C và càng ít mức độ thấp bạn có thể thoải mái đến với yêu cầu HTTP, Poco và Casablanca cung cấp các giao diện C++ hợp ngôn ngữ mà một số người có thể ưa thích.

Bên trong cỗ máy, khi bạn tải xuống một trang web, giao thức HTTP được kích hoạt. Một yêu cầu GET được gửi đến máy chủ, và giả sử mọi thứ diễn ra tốt đẹp, máy chủ phản hồi với nội dung được gói trong một phản hồi HTTP.

## Xem thêm
- [Trang chính thức của libcurl](https://curl.se/libcurl/)
- [Repo GitHub C++ Rest SDK](https://github.com/microsoft/cpprestsdk)
- [Dự án Poco](https://pocoproject.org/)
- [HTTP trên Wikipedia](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
