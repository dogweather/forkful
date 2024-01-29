---
title:                "Tải trang web"
date:                  2024-01-28T21:59:04.920590-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tải trang web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tải xuống một trang web có nghĩa là lấy nội dung HTML của nó từ máy chủ web nơi nó cư trú. Lập trình viên làm điều này để xử lý, phân tích hoặc tương tác với dữ liệu trang web ngoại tuyến.

## Làm thế nào:
```C
#include <stdio.h>
#include <stdlib.h>
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
    char outfilename[FILENAME_MAX] = "downloaded_page.html";

    curl = curl_easy_init();
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        
        res = curl_easy_perform(curl);
        /* Kiểm tra lỗi */
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() thất bại: %s\n",
                curl_easy_strerror(res));
        
        /* Dọn dẹp */
        curl_easy_cleanup(curl);
        fclose(fp);
    }

    return 0;
}
```
Đầu ra mẫu:
```
(Không có đầu ra, nhưng kiểm tra thư mục hiện tại cho tệp 'downloaded_page.html')
```

## Đi sâu vào vấn đề
Trở lại những ngày đầu của internet, việc nắm bắt một trang web liên quan đến các yêu cầu HTTP thô thông qua các socket TCP - cồng kềnh, nói ít nhất. Ngày nay, chúng ta có các thư viện như libcurl, giúp giảm bớt công việc. Nó xử lý tất cả những vấn đề phức tạp của yêu cầu HTTP, kết nối SSL, và nhiều hơn nữa.

Có một số lựa chọn thay thế cho libcurl như wget và http-client trong C, nhưng libcurl được sử dụng rộng rãi vì độ bền vững và các tính năng của nó. Khi sử dụng libcurl, hãy nhớ:

- Khởi tạo với `curl_easy_init()` là bắt buộc.
- Đặt các tùy chọn phù hợp với nhu cầu của bạn; để tải xuống, chúng ta cần chỉ định URL và hàm ghi.
- `CURLOPT_WRITEFUNCTION` cho phép chúng ta truyền một con trỏ đến hàm gọi lại của mình để viết dữ liệu vào tệp.
- Luôn kiểm tra kết quả của `curl_easy_perform()` để tìm lỗi.
- Đừng quên dọn dẹp với `curl_easy_cleanup()` để ngăn chặn rò rỉ.

Đối với mã sản phẩm, bạn sẽ muốn xử lý lỗi, kiểm tra mã trạng thái HTTP, và quản lý các vấn đề an ninh (như xác thực chứng chỉ SSL).

## Xem thêm
- [libcurl](https://curl.se/libcurl/)
- [HTTP Made Really Easy](https://www.jmarshall.com/easy/http/)
