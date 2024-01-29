---
title:                "Gửi một yêu cầu HTTP"
date:                  2024-01-28T22:07:54.014059-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Gửi một yêu cầu HTTP là cách chương trình của bạn yêu cầu dữ liệu hoặc gửi dữ liệu đến một máy chủ web. Lập trình viên sử dụng nó để tương tác với các API, thu thập nội dung web hoặc giao tiếp với các dịch vụ khác.

## Làm thế nào:

Trong C, chúng ta sẽ sử dụng `libcurl` cho nhiệm vụ này. Đây là một thư viện mạnh mẽ để chuyển dữ liệu với các URL. Đầu tiên, cài đặt `libcurl`. Trên các hệ thống dựa trên Debian, bạn có thể sử dụng `sudo apt-get install libcurl4-openssl-dev`.

Dưới đây là một đoạn mã để thực hiện một yêu cầu GET đơn giản:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        
        /* Thực hiện yêu cầu, res sẽ nhận mã trả về */ 
        res = curl_easy_perform(curl);
        
        /* Kiểm tra lỗi */ 
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() đã thất bại: %s\n",
                    curl_easy_strerror(res));
        
        /* luôn luôn dọn dẹp */ 
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Khi chạy điều này, bạn sẽ không nhìn thấy đầu ra hiển thị vì chúng ta chưa xử lý phản hồi, nhưng nó sẽ tải nội dung từ `http://example.com`.

## Sâu hơn

`libcurl` được bắt đầu vào năm 1997 và đã trở thành thư viện ưa thích của các lập trình viên C. Nó hỗ trợ một số lượng lớn các giao thức, không chỉ HTTP. Các phương án thay thế cho HTTP trong C có thể bao gồm việc tự viết thực hiện của riêng bạn, nhưng đó là một hành trình gập ghềnh qua lập trình socket và các RFC phức tạp.

`libcurl` tiện lợi vì nó xử lý tất cả các chi tiết phức tạp cho bạn, như thương lượng giao thức, xử lý lỗi và chuyển dữ liệu. Hơn nữa, nó là nền tảng chéo - sử dụng cùng một mã trên Linux, Windows, Mac, bạn tên nó.

Hãy nhớ, `libcurl` sử dụng một API đồng bộ theo mặc định, có thể sẽ chặn luồng chính của bạn. Nếu bạn đang xây dựng thứ gì đó mà điều này quan trọng, bạn có thể phải đào sâu vào đa luồng hoặc bộ chức năng `curl_multi_*` đồng bộ.

## Xem thêm

- Trang web chính thức của libcurl để xem tài liệu và ví dụ: [https://curl.se/libcurl/](https://curl.se/libcurl/)
- Chi tiết giao thức HTTP để có kiến thức nền: [https://www.ietf.org/rfc/rfc2616.txt](https://www.ietf.org/rfc/rfc2616.txt)
- Để hiểu rộng hơn về lập trình mạng C: [Hướng dẫn về Lập trình Mạng của Beej](https://beej.us/guide/bgnet/)
