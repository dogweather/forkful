---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:58.812070-07:00
description: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1\
  \ b\u1EA3n bao g\u1ED3m vi\u1EC7c truy\u1EC1n t\xEAn \u0111\u0103ng nh\u1EADp v\xE0\
  \ m\u1EADt kh\u1EA9u \u0111\u1EC3 x\xE1c nh\u1EADn danh t\xEDnh c\u1EE7a ng\u01B0\
  \u1EDDi d\xF9ng. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n\u2026"
lastmod: '2024-03-13T22:44:36.878796-06:00'
model: gpt-4-0125-preview
summary: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1\
  \ b\u1EA3n bao g\u1ED3m vi\u1EC7c truy\u1EC1n t\xEAn \u0111\u0103ng nh\u1EADp v\xE0\
  \ m\u1EADt kh\u1EA9u \u0111\u1EC3 x\xE1c nh\u1EADn danh t\xEDnh c\u1EE7a ng\u01B0\
  \u1EDDi d\xF9ng. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n\u2026"
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

## Cái gì & Tại sao?
Gửi một yêu cầu HTTP với xác thực cơ bản bao gồm việc truyền tên đăng nhập và mật khẩu để xác nhận danh tính của người dùng. Các lập trình viên thực hiện việc này để truy cập vào những tài nguyên bị hạn chế trên máy chủ, đảm bảo một số mức độ bảo mật.

## Làm thế nào:

Hãy thử nghiệm với một số mã. Chúng ta sẽ sử dụng `curl`, một công cụ dòng lệnh phổ biến. Thay thế `username:password` bằng thông tin xác thực của bạn và `http://example.com/resource` bằng URL mục tiêu của bạn.

```Bash
curl -u username:password http://example.com/resource
```

Hoặc mã hóa thông tin xác thực của bạn bằng base64 trước và sử dụng như sau:

```Bash
# Mã hóa thông tin xác thực
credentials=$(echo -n username:password | base64)

# Gửi yêu cầu
curl -H "Authorization: Basic $credentials" http://example.com/resource
```

Kết quả mẫu cho một yêu cầu thành công có thể trông như thế này:

```Bash
{
  "data": "Một số thông tin bị hạn chế",
  "message": "Truy cập được chấp nhận"
}
```

## Sâu hơn

Theo lịch sử, xác thực cơ bản đã là một phần của HTTP từ những ngày đầu, nhưng nó không phải không có nhược điểm - chủ yếu là sự dễ bị tấn công nếu không sử dụng qua một kênh an toàn như HTTPS.

Các lựa chọn thay thế bao gồm OAuth, được coi là an toàn hơn và cung cấp kiểm soát tốt hơn về những gì được truy cập. Xác thực Digest là một lựa chọn khác, gửi thông tin xác thực được băm thay vì văn bản thuần.

Về cơ học, khi bạn gửi thông tin xác thực cơ bản, chúng được bao gồm trong tiêu đề HTTP được mã hóa trong Base64. Đây không phải là mã hóa, vì vậy nếu bạn không sử dụng HTTPS, bất kỳ ai chặn được yêu cầu đều có thể giải mã một cách dễ dàng. Sử dụng HTTPS bảo vệ việc truyền dữ liệu, mã hóa mọi thứ giữa khách hàng và máy chủ.

## Xem thêm

- Tài liệu chính thức của cURL: https://curl.haxx.se/docs/manpage.html
- Xác thực HTTP: Xác thực Truy cập Cơ bản và Digest (IETF RFC 7617): https://tools.ietf.org/html/rfc7617
- Giới thiệu về OAuth: https://oauth.net/2/introduction/
