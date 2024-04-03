---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:58.812070-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y th\u1EED nghi\u1EC7m v\u1EDBi m\u1ED9\
  t s\u1ED1 m\xE3. Ch\xFAng ta s\u1EBD s\u1EED d\u1EE5ng `curl`, m\u1ED9t c\xF4ng\
  \ c\u1EE5 d\xF2ng l\u1EC7nh ph\u1ED5 bi\u1EBFn. Thay th\u1EBF `username:password`\
  \ b\u1EB1ng th\xF4ng tin x\xE1c\u2026"
lastmod: '2024-03-13T22:44:36.878796-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y th\u1EED nghi\u1EC7m v\u1EDBi m\u1ED9t s\u1ED1 m\xE3."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

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
