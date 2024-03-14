---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:43.815296-07:00
description: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1\
  \ b\u1EA3n trong C \u0111\xF2i h\u1ECFi vi\u1EC7c t\u1EA1o ra m\u1ED9t y\xEAu c\u1EA7\
  u HTTP bao g\u1ED3m m\u1ED9t ti\xEAu \u0111\u1EC1 \u1EE6y quy\u1EC1n v\u1EDBi th\xF4\
  ng tin \u0111\u0103ng nh\u1EADp c\u1EE7a ng\u01B0\u1EDDi d\xF9ng\u2026"
lastmod: '2024-03-13T22:44:37.267427-06:00'
model: gpt-4-0125-preview
summary: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1\
  \ b\u1EA3n trong C \u0111\xF2i h\u1ECFi vi\u1EC7c t\u1EA1o ra m\u1ED9t y\xEAu c\u1EA7\
  u HTTP bao g\u1ED3m m\u1ED9t ti\xEAu \u0111\u1EC1 \u1EE6y quy\u1EC1n v\u1EDBi th\xF4\
  ng tin \u0111\u0103ng nh\u1EADp c\u1EE7a ng\u01B0\u1EDDi d\xF9ng\u2026"
title: "G\u1EEDi y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3n"
---

{{< edit_this_page >}}

## Là gì và Tại sao?
Gửi một yêu cầu HTTP với xác thực cơ bản trong C đòi hỏi việc tạo ra một yêu cầu HTTP bao gồm một tiêu đề Ủy quyền với thông tin đăng nhập của người dùng được mã hóa trong Base64. Đây là một phương pháp phổ biến để thêm một lớp xác thực đơn giản vào các yêu cầu HTTP, cho phép truy cập vào những tài nguyên bị hạn chế một cách lập trình.

## Làm thế nào:
Để gửi một yêu cầu HTTP với xác thực cơ bản trong C, chúng ta sẽ cần sử dụng thư viện libcurl, một thư viện truyền tải URL phía client phổ biến, linh hoạt và dễ sử dụng. Nó xử lý các giao thức khác nhau, bao gồm HTTP và HTTPS, làm cho nhiệm vụ của chúng ta trở nên đơn giản hơn. Hãy đảm bảo libcurl đã được cài đặt trong hệ thống của bạn trước khi tiếp tục. Dưới đây là một ví dụ cơ bản cho thấy cách gửi một yêu cầu GET với xác thực cơ bản:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        // URL mà yêu cầu được gửi đến
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        // Kích hoạt sử dụng xác thực cơ bản
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        // Cung cấp tên người dùng và mật khẩu cho xác thực cơ bản
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

        // Thực hiện yêu cầu GET
        res = curl_easy_perform(curl);

        // Kiểm tra lỗi
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));

        // Luôn luôn dọn dẹp
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();

    return 0;
}
```
Trong ví dụ trên, thay thế `"http://example.com/resource"`, `"username"`, và `"password"` bằng URL, tên người dùng và mật khẩu thực tế của bạn.

Đoạn mã này khởi tạo một đối tượng `CURL`, thiết lập URL, kích hoạt Xác thực HTTP Cơ bản và chỉ định các thông tin xác thực. Sau đó, nó gửi yêu cầu và dọn dẹp sau khi thực hiện xong. Nếu thành công, tài nguyên được yêu cầu sẽ được tải về; nếu có lỗi, nó sẽ được in ra stderr.

Kết quả mẫu (giả sử xác thực thành công và truy cập tài nguyên thành công) có thể không được chương trình hiển thị trực tiếp, vì ví dụ chủ yếu là để thể hiện việc gửi yêu cầu. Để in phản hồi, bạn sẽ mở rộng chương trình để xử lý dữ liệu phản hồi HTTP.

## Sâu hơn:
Gửi yêu cầu HTTP với xác thực cơ bản trong C, như đã được trình bày, tận dụng thư viện libcurl vì độ tin cậy và sự đơn giản của nó. Trong lịch sử, việc tạo ra các yêu cầu HTTP một cách thuần túy trong C mà không cần đến các thư viện như vậy là rất khó khăn và dễ mắc lỗi, bao gồm cả việc lập trình socket ở mức độ thấp và việc xây dựng thủ công các tiêu đề HTTP.

Chính xác thực cơ bản là một phương pháp từ những ngày đầu của web. Nó gửi thông tin xác thực trong một định dạng dễ giải mã (Base64), mà bản thân nó không an toàn trên các kênh văn bản. Ứng dụng hiện đại thường ưu tiên các phương pháp xác thực an toàn hơn, như OAuth 2.0 hoặc JWT (JSON Web Tokens), đặc biệt là cho dữ liệu nhạy cảm.

Tuy nhiên, đối với hệ thống nội bộ ít quan trọng hơn, hoặc các kịch bản nhanh và bẩn mà sự tiện lợi lấn át mối quan tâm về an ninh, xác thực cơ bản vẫn được sử dụng. Hơn nữa, khi kết hợp với các kết nối được mã hóa (HTTPS), sự đơn giản của nó trở thành lợi thế cho việc phát triển nhanh, thử nghiệm hoặc công việc tự động hóa khi cơ chế bảo mật cấp cao hơn không thực sự cần thiết.

Trong các bối cảnh mà bảo mật hàng đầu không thể thương lượng, các phương thức xác thực dựa trên token nên được ưu tiên. Tuy nhiên, việc hiểu cách triển khai xác thực cơ bản trong C thông qua libcurl cung cấp một kỹ năng cơ bản có thể được điều chỉnh cho các phương thức xác thực và giao thức khác nhau, phản ánh sự cân nhắc tinh tế giữa an ninh, sự tiện lợi và yêu cầu ứng dụng trong phát triển web.
