---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:30.762761-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Bash c\xF3 th\u1EC3 s\u1EED d\u1EE5ng c\xE1\
  c c\xF4ng c\u1EE5 nh\u01B0 `curl` ho\u1EB7c `wget` cho c\xE1c y\xEAu c\u1EA7u HTTP.\
  \ D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 nhanh v\u1EDBi `curl`."
lastmod: '2024-03-13T22:44:36.875084-06:00'
model: gpt-4-0125-preview
summary: "Bash c\xF3 th\u1EC3 s\u1EED d\u1EE5ng c\xE1c c\xF4ng c\u1EE5 nh\u01B0 `curl`\
  \ ho\u1EB7c `wget` cho c\xE1c y\xEAu c\u1EA7u HTTP."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

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
