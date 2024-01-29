---
title:                "Gửi một yêu cầu HTTP"
date:                  2024-01-28T22:07:30.762761-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Gửi một yêu cầu HTTP là cách để giao tiếp với các máy chủ web nhằm truy xuất dữ liệu hoặc gửi biểu mẫu. Lập trình viên thực hiện việc này để tương tác với các dịch vụ web, API hoặc tự động hóa các nhiệm vụ liên quan đến nội dung web.

## Cách thực hiện:

Bash có thể sử dụng các công cụ như `curl` hoặc `wget` cho các yêu cầu HTTP. Dưới đây là một ví dụ nhanh với `curl`.

```Bash
# Lấy nội dung của một trang web
curl https://example.com

# Gửi dữ liệu lên máy chủ
curl -d "param1=value1&param2=value2" -X POST https://example.com/post-endpoint

# Bao gồm tiêu đề trong một yêu cầu GET
curl -H "Content-Type: application/json" https://example.com
```

Mẫu phản hồi `curl`:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Nghiên cứu sâu

Yêu cầu HTTP đã tồn tại từ đầu những năm '90 và là nền tảng của giao tiếp web. `curl` và `wget` là các công cụ dòng lệnh Unix được giới thiệu vào năm 1996 và 1996, tương ứng, cho các yêu cầu mạng.

`wget` thường được sử dụng để tải xuống tệp, trong khi `curl` có thể xử lý một loạt các giao thức và cung cấp nhiều tính năng hơn, làm cho nó trở thành lựa chọn hàng đầu để gửi yêu cầu HTTP từ dòng lệnh.

Thực hiện một yêu cầu HTTP bằng cách sử dụng những công cụ này bao gồm việc tạo ra các tiêu đề yêu cầu, phương thức (GET, POST, PUT, DELETE, v.v.) phù hợp và đôi khi là các trọng tải dữ liệu. Thực hiện điều này từ các kịch bản Bash cho phép tự động hóa tương tác với các dịch vụ dựa trên web.

Các cách thay thế để gửi yêu cầu HTTP trong lập trình kịch bản bao gồm sử dụng các ngôn ngữ kịch bản như Python với các thư viện như `requests`, hoặc sử dụng các công cụ như `httpie` cho giao diện thân thiện với người dùng hơn.

## Xem thêm

- Trang chính thức của curl: https://curl.se/
- Hướng dẫn sử dụng wget: https://www.gnu.org/software/wget/manual/wget.html
- HTTPie: https://httpie.io/
- Học viện Bash: https://www.bash.academy/
- Quy cách HTTP của W3C: https://www.w3.org/Protocols/
