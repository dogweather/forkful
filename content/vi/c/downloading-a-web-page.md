---
title:                "Tải trang web về"
aliases:
- vi/c/downloading-a-web-page.md
date:                  2024-02-03T17:56:23.794694-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tải trang web về"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/downloading-a-web-page.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?

Tải một trang web bằng C bao gồm việc truy cập trực tiếp vào nội dung của một trang web trên internet và lưu nó tại máy cục bộ để xử lý hoặc sử dụng ngoại tuyến. Các lập trình viên thường tham gia vào việc này để tiêu thụ dịch vụ web, kéo nội dung web, hoặc tương tác trực tiếp với các nguồn tài nguyên trực tuyến từ các ứng dụng của họ.

## Cách thực hiện:

Để tải một trang web trong C, một phương pháp phổ biến là sử dụng thư viện libcurl, một thư viện chuyển giao URL bên client hiệu quả và có thể chuyển đổi. Hãy đảm bảo rằng bạn đã cài đặt và liên kết libcurl trong dự án của mình. Dưới đây là một ví dụ minh hoạ cách sử dụng libcurl để tải nội dung của một trang web:

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "./downloaded_page.html";

    curl = curl_easy_init(); // Khởi tạo một phiên libcurl dễ dàng
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // Hàm gọi lại để ghi dữ liệu nhận được
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // Thiết lập con trỏ tập tin để ghi dữ liệu vào

        res = curl_easy_perform(curl); // Thực hiện tải tập tin
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        }

        /* luôn luôn dọn dẹp */
        curl_easy_cleanup(curl); // Dọn dẹp phiên dễ dàng
        fclose(fp); // Đóng luồng tập tin
    }
    return 0;
}
```
Kết quả mẫu (không có đầu ra hiển thị trong console): Mã này tải nội dung tại URL được chỉ định và lưu vào một tệp có tên là `downloaded_page.html`. Kiểm tra thư mục của chương trình của bạn để xem nội dung đã tải xuống.

## Tìm hiểu sâu:

Trong quá khứ, việc tải nội dung web trong C là khó khăn hơn nhiều, yêu cầu lập trình socket thủ công và xử lý giao thức HTTP. Libcurl tóm tắt những phức tạp này, cung cấp một API cấp cao và mạnh mẽ cho việc chuyển dữ liệu qua web.

Mặc dù libcurl làm đơn giản việc gửi yêu cầu HTTP trong C, ngôn ngữ lập trình hiện đại như Python với thư viện `requests` của họ hoặc JavaScript (Node.js) với nhiều thư viện client HTTP có thể cung cấp cú pháp trực quan hơn và hỗ trợ tích hợp cho JSON và các định dạng dữ liệu khác thường được sử dụng trong giao tiếp web. Tuy nhiên, C và libcurl cung cấp một giải pháp ổn định và hiệu suất cao cho các hệ thống nơi hiệu quả, kiểm soát chi tiết, hoặc tích hợp vào cơ sở mã C hiện có là quan trọng. Đáng chú ý là C, kết hợp với libcurl, có thể được sử dụng không chỉ để tải trang web - nó còn hỗ trợ FTP, SMTP, và nhiều hơn nữa, làm cho nó trở thành một công cụ đa năng trong bộ công cụ của một lập trình viên.
