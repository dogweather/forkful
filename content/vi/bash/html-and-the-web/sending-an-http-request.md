---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:30.762761-07:00
description: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP l\xE0 c\xE1ch \u0111\u1EC3 giao\
  \ ti\u1EBFp v\u1EDBi c\xE1c m\xE1y ch\u1EE7 web nh\u1EB1m truy xu\u1EA5t d\u1EEF\
  \ li\u1EC7u ho\u1EB7c g\u1EEDi bi\u1EC3u m\u1EABu. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 t\u01B0\u01A1ng t\xE1c v\u1EDBi\u2026"
lastmod: '2024-03-11T00:14:10.164727-06:00'
model: gpt-4-0125-preview
summary: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP l\xE0 c\xE1ch \u0111\u1EC3 giao ti\u1EBF\
  p v\u1EDBi c\xE1c m\xE1y ch\u1EE7 web nh\u1EB1m truy xu\u1EA5t d\u1EEF li\u1EC7\
  u ho\u1EB7c g\u1EEDi bi\u1EC3u m\u1EABu. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n vi\u1EC7c n\xE0y \u0111\u1EC3 t\u01B0\u01A1ng t\xE1c v\u1EDBi\u2026"
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
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
